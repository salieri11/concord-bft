%% Copyright 2018 VMware, all rights reserved.
%%
%% A connection to an Athena node.
%%
%% This gen_server maintains a connection to one athena node. It
%% buffers requests locally, sending one request to Athena at a time,
%% and waiting for its response before sending the next. (TODO: allow
%% multiple outstanding requests)
%%
%% TODO: timeouts
%% TODO: liveness ping

-module(helen_athena_conn).

-behaviour(gen_server).

-include("athena_pb.hrl").

%% API
-export([
         start_link/1,
         send_echo_request/1,
         send_request/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CLIENT_VERSION, 1).
-define(QUEUE_LIMIT, 10).

%% Delay to add after the first reconnect attempt.
-define(BASE_CONNECT_DELAY_MS, 10).
%% We'll double the base connection delay up to this amount.
-define(MAX_CONNECT_DELAY_MS, 10000).

-record(buffer, {
          wait,
          chunks = {[], []}
}).

-record(state, {
          node :: node_address(),
          socket :: inet:socket(),
          queue :: queue:queue(),
          from :: from(),
          buffer = #buffer{},
          reconnect_delay = 0 :: non_neg_integer()
}).

-type athenarequest() :: #athenarequest{}.
-type athenaresponse() :: #athenaresponse{}.
-type from() :: {pid(),term()}|self_version.
-type node_address() :: {inet:ip_address(), inet:port_number()}.
-type buffer() :: #buffer{}.
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% Starts the server.
-spec start_link(node_address()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Node) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Node, []).

%% Ask Athena to echo a string back. It is the caller's responsibility
%% to check whether or not the returned string matches the input
%% string.
-spec send_echo_request(binary()) -> {ok|error, binary()}.
send_echo_request(String) ->
    Msg = #athenarequest{
             test_request =
                 #testrequest{
                    echo = String
                   }},
    case send_request(Msg) of
        #athenaresponse{test_response=T} when T /= undefined ->
            case T#testresponse.echo of
                undefined ->
                    {error, <<"no echo string">>};
                Echo ->
                    {ok, Echo}
            end;
        #athenaresponse{} ->
            {error, <<"no TestResponse">>};
        _ ->
            {error, <<"invalid response">>}
    end.

%% Send a request to Athena.
-spec send_request(athenarequest()) ->
          athenaresponse()|{error, binary()}.
send_request(#athenarequest{}=Msg) ->
    case catch gen_server:call(?SERVER, {send, Msg}) of
        {'EXIT', {timeout, _}} ->
            {error, <<"timeout">>};
        Other ->
            Other
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Initializes the server. Begin async reconnect process.
-spec init(node_address()) -> {ok, state()} |
                              {ok, state(), timeout()} |
                              ignore |
                              {stop, term()}.
init(Node) ->
    {ok, reconnect(#state{node=Node, queue=queue:new()})}.

%% Handling call messages. This is currently just "new request".
-spec handle_call(term(), from(), state()) ->
          {reply, athenaresponse()|{error, binary()}, state()} |
          {reply, athenaresponse()|{error, binary()}, state(), timeout()} |
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, term(), athenaresponse()|{error, binary()}, state()} |
          {stop, term(), state()}.
handle_call({send, #athenarequest{}=Msg}, From, State) ->
    new_request(Msg, From, State);
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% Handling cast messages.
-spec handle_cast(term(), state()) -> {noreply, state()} |
                                      {noreply, state(), timeout()} |
                                      {stop, term(), state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handling all non call/cast messages. Two types are expected:
%%
%%  1) TCP socket messages, because this module uses {active, once} to
%%  read asynchronously.
%%
%%  2) Reconnect requests. We do these asynchronously, to allow the
%%  init function to return quickly, and so that any state can safely
%%  initiate teardown and reconnection.
-spec handle_info(term(), state()) -> {noreply, state()} |
                                      {noreply, state(), timeout()} |
                                      {stop, term(), state()}.
handle_info({tcp, Socket, Data}, #state{socket=Socket}=State) ->
    {noreply, new_data(Data, State)};
handle_info({tcp_closed, Socket}, #state{socket=Socket}=State) ->
    {noreply, reconnect(State)};
handle_info({tcp_error, Socket, Reason}, #state{socket=Socket}=State) ->
    log(error_msg, State, "Socket disconnected (~p)", [Reason]),
    {noreply, reconnect(State)};
handle_info(reconnect, State) ->
    %% first close any socket we have open
    Disconnected = disconnect(State),
    Connected = connect(Disconnected),
    {noreply, Connected};
handle_info(_Info, State) ->
    {noreply, State}.

%% Server is cleaning up. Tear down the connection.
-spec terminate(term(), state()) -> term().
terminate(_Reason, State) ->
    _ = disconnect(State),
    ok.

%% Convert process state when code is changed
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Just trigger a future reconnect, handled async.
-spec reconnect(state()) -> state().
reconnect(#state{reconnect_delay=0}=State) ->
    self() ! reconnect,
    State#state{reconnect_delay=?BASE_CONNECT_DELAY_MS};
reconnect(#state{reconnect_delay=Delay}=State) ->
    erlang:send_after(Delay, self(), reconnect),
    NewDelay = min(Delay*2, ?MAX_CONNECT_DELAY_MS),
    State#state{reconnect_delay=NewDelay}.

%% Close the connection to Athena. And abort inflight requests.
-spec disconnect(state()) -> state().
disconnect(#state{socket=undefined}=State) ->
    State;
disconnect(#state{socket=Socket}=State) ->
    _ = gen_tcp:close(Socket),
    Cleared = abort_inflight(State),
    Cleared#state{socket=undefined}.

%% Send an error to the sender of any request that has been forwarded
%% to Athena, since we won't receive the response. Clear the receive
%% buffer.
-spec abort_inflight(state()) -> state().
abort_inflight(State) ->
    forward_response({error, <<"connection closed">>}, State),
    State#state{from=undefined, buffer=#buffer{}}.

%% Open the connection to Athena. Purposefully raises an exception if
%% a connection is already open. After a connection is opened, begin a
%% protocol request to share version numbers.
-spec connect(state()) -> state().
connect(#state{socket=undefined, node={IP, Port}}=State) ->
    case catch gen_tcp:connect(IP, Port, [binary,{active,false}]) of
        {ok, Socket} ->
            Connected = State#state{socket=Socket},
            send_version(Connected);
        {_ErrorOrExit, Reason} when _ErrorOrExit == error;
                                    _ErrorOrExit == 'EXIT' ->
            log(error_msg, State,
                "Unable to connect to athena at (~p)", [Reason]),
            reconnect(State)
    end.

%% Send the client version. Begins an async wait for server version
%% response.
-spec send_version(state()) -> state().
send_version(State) ->
    Msg = #athenarequest{
             protocol_request =
                 #protocolrequest{client_version = ?CLIENT_VERSION}},
    send(Msg, self_version, State).

%% Send an AthenaRequest to Athena.
-spec send(athenarequest(), from(), state()) -> state().
send(#athenarequest{}=Msg, From, #state{socket=Socket}=State)
  when Socket /= undefined ->
    Encoded = athena_pb:encode(Msg),
    %% TODO: send error if message is too large
    Prefix = <<(iolist_size(Encoded)):2/little-unsigned-integer-unit:8>>,
    case gen_tcp:send(Socket, [Prefix, Encoded]) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            State#state{from=From};
        {error, Reason} ->
            log(error_msg, State, "Unable to send to (~p)", [Reason]),
            %% reconnect sends an error message to the waiting client
            reconnect(State#state{from=From})
    end.

%% Send a request to Athena, or queue for send later if a request is
%% already outstanding.
-spec new_request(athenarequest(), from(), state()) ->
          {noreply, state()} |
          {reply, {error, binary()}, state()}.
new_request(#athenarequest{}=Msg, From,
            #state{from=undefined, socket=Socket}=State)
  when Socket /= undefined ->
    {noreply, send(Msg, From, State)};
new_request(#athenarequest{}=Msg, From, State) ->
    case queue_request(Msg, From, State) of
        {ok, NewState} ->
            {noreply, NewState};
        Error ->
            {reply, Error, State}
    end.

%% Queue a request to Athena, if there are not too many queued already.
-spec queue_request(athenarequest(), from(), state()) ->
          {ok, state()} | {error, binary()}.
queue_request(Msg, From, #state{queue=Q}=State) ->
    case queue:len(Q) >= ?QUEUE_LIMIT of
        false ->
            {ok, State#state{queue=queue:in({Msg, From}, Q)}};
        true ->
            {error, <<"overload">>}
    end.

%% Send the next request in the queue to Athena (if there is one).
-spec dequeue_request(state()) -> state().
dequeue_request(#state{queue=Q}=State) ->
    case queue:out(Q) of
        {{value, {Msg, From}}, NewQ} ->
            send(Msg, From, State#state{queue=NewQ});
        {empty, _} ->
            State
    end.

%% Handle new TCP data received. Buffer it until a full message is
%% ready. When a full message has been read, send the response to the
%% caller, and dequeue the next request.
-spec new_data(binary(), state()) -> state().
new_data(<<>>, #state{socket=Socket}=State) ->
    %% filter zero-length packets
    inet:setopts(Socket, [{active, once}]),
    State;
new_data(Data, #state{buffer=Buffer, socket=Socket}=State) ->
    case find_response(Data, Buffer) of
        {ok, Response, NewBuffer} ->
            dequeue_request(
              forward_response(Response, State#state{buffer=NewBuffer}));
        {wait, NewBuffer} ->
            inet:setopts(Socket, [{active, once}]),
            State#state{buffer=NewBuffer}
    end.

%% Attempt to read a response out of the data received so far.
-spec find_response(binary(), buffer()) ->
          {ok, athenaresponse(), buffer()} | {wait, buffer()}.
find_response(Data, #buffer{}=Buffer) ->
    case find_length(Data, Buffer) of
        {ok, NewBuffer} ->
            try_decode(NewBuffer);
        {wait, NewBuffer} ->
            {wait, NewBuffer}
    end.

%% Attempt to read the length of the next message from the data
%% received so far. Returns 'ok' if the length was read, or had
%% already been read in previously-received data. Returns 'wait' if no
%% length has been read yet.
-spec find_length(binary(), buffer()) -> {ok|wait, buffer()}.
find_length(NewData, #buffer{wait=undefined, chunks=Chunks}=B) ->
    case get_bytes(2, Chunks, NewData) of
        {ok, [<<Length:2/little-unsigned-integer-unit:8>>], NewChunks} ->
            {ok, B#buffer{wait=Length, chunks=NewChunks}};
        {ok, [_,_]=List, NewChunks} ->
            <<Length:2/little-unsigned-integer-unit:8>> =
                iolist_to_binary(List),
            {ok, B#buffer{wait=Length, chunks=NewChunks}};
        {wait, NewChunks} ->
            {wait, B#buffer{chunks=NewChunks}}
    end;
find_length(NewData, #buffer{chunks={HeadChunks, TailChunks}}=B) ->
    {ok, B#buffer{chunks={HeadChunks, [NewData|TailChunks]}}}.

%% Try to decode a response from the data already received. Length
%% should have already been read - this function will bad-match if it
%% hasn't been.
-spec try_decode(buffer()) -> {ok, athenaresponse(), buffer()} |
                              {wait, buffer}.
try_decode(#buffer{wait=Length, chunks=Chunks}=B) when length /= undefined ->
    case get_bytes(Length, Chunks) of
        {ok, Bytes, NewChunks} ->
            %% TODO: this iolist_to_binary is aggravating
            {ok, athena_pb:decode_athenaresponse(iolist_to_binary(Bytes)),
             B#buffer{wait=undefined, chunks=NewChunks}};
        {wait, NewChunks} ->
            {wait, B#buffer{chunks=NewChunks}}
    end.

%% Send the response to the caller. If the response was for our
%% initial protocol request to share versions, verify the result, and
%% issue a reconnect if it was invalid.
-spec forward_response(athenaresponse()|{error, binary()}, state()) -> state().
forward_response(_Response, #state{from=undefined}=State) ->
    %% response is an error because the connection closed, but there
    %% was no outstanding client request, so we don't need to do
    %% anything
    State;
forward_response(Response, #state{from=self_version}=State) ->
    case Response of
        #athenaresponse{protocol_response=P} ->
            case P of
                #protocolresponse{server_version=V} when V /= undefined ->
                    log(info_msg, State, "Connected to server version ~p", [V]),
                    State#state{from=undefined, reconnect_delay=0};
                _ ->
                    log(error_msg, State, "Server did not reply with version"),
                    reconnect(State#state{from=undefined})
            end;
        {error, _} ->
            log(error_msg, State, "Invalid server response"),
            %% something else is already calling reconnect in this case
            State#state{from=undefined}
    end;
forward_response(Response, #state{from=From}=State) ->
    gen_server:reply(From, Response),
    State#state{from=undefined}.

%% Implementation of a basic queue buffer. Chunks are added to
%% TailChunks, which remains in reverse order (head is most recently
%% received) until HeadChunks is empty, and which point it is reversed
%% and used as the new HeadChunks. This function first adds the new
%% chunk in the right place, then calls real extraction.
-spec get_bytes(integer(), {[binary()], [binary()]}, binary()) ->
          {ok, [binary()], {[binary()], [binary()]}} |
          {wait, {[binary()], [binary()]}}.
get_bytes(Count, {[], TailChunks}, NewChunk) ->
    get_bytes(Count, {lists:reverse([NewChunk|TailChunks]), []});
get_bytes(Count, {HeadChunks, TailChunks}, NewChunk) ->
    get_bytes(Count, {HeadChunks, [NewChunk|TailChunks]}).

%% Actual extraction from the buffer.
-spec get_bytes(integer(), {[binary()], [binary()]}) ->
          {ok, [binary()], {[binary()], [binary()]}} |
          {wait, {[binary()], [binary()]}}.
get_bytes(Count, {[Head|Chunks]=HC, TailChunks}=C) ->
    case Head of
        <<Bytes:Count/binary>> ->
            %% exactly what was needed in first chunk
            {ok, [Bytes], {Chunks, TailChunks}};
        <<Bytes:Count/binary, Rest/binary>> ->
            %% more than what was needed in first chunk
            {ok, [Bytes], {[Rest|Chunks], TailChunks}};
        _ ->
            %% not enough in first chunk
            case Count < (iolist_size(HC) + iolist_size(TailChunks)) of
                true ->
                    %% we have enough data - dig in
                    {ok, Bytes, Rest} = get_bytes(Count-size(Head),
                                                  {HC, TailChunks}),
                    {ok, [Head|Bytes], Rest};
                false ->
                    %% we don't have enough data - wait for more
                    {wait, C}
            end
    end;
get_bytes(Count, {[], []}=Empty) ->
    {wait, Empty};
get_bytes(Count, {[], TailChunks}) ->
    get_bytes(Count, {lists:reverse(TailChunks), []}).

%% Simply log wrapper, so we can easily include the node address in
%% all logs.
-spec log(info_msg|warning_msg|error_msg, state(), string()) -> term().
log(Level, State, Message) ->
    log(Level, State, Message, []).

%% Simply log wrapper, so we can easily include the node address in
%% all logs.
-spec log(info_msg|warning_msg|error_msg, state(), string(), [term()]) ->
          term().
log(Level, #state{node=N}, Message, Params) ->
    error_logger:Level("~p: "++Message, [N|Params]).
