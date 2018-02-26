%% Copyright 2018 VMware, all rights reserved.
%%
%% A connection to an Athena node.
%%
%% TODO: timeouts

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

-record(buffer, {
          wait,
          chunks = {[], []}
}).

-record(state, {
          node,
          socket,
          queue,
          from,
          buffer = #buffer{}
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(term()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Node) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Node, []).

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

send_request(#athenarequest{}=Msg) ->
    gen_server:call(?SERVER, {send, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Node) ->
    {ok, reconnect(#state{node=Node, queue=queue:new()})}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send, #athenarequest{}=Msg}, From, State) ->
    new_request(Msg, From, State);
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(reconnect, State) ->
    %% first close any socket we have open
    Disconnected = disconnect(State),
    Connected = connect(Disconnected),
    {noreply, Connected};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, #state{socket=Socket}=State) ->
    {noreply, new_data(Data, State)};
handle_info({tcp_closed, Socket}, #state{socket=Socket}=State) ->
    {noreply, reconnect(State)};
handle_info({tcp_error, Socket, Reason}, #state{socket=Socket}=State) ->
    log(error, State, "Socket disconnected (~p)", [Reason]),
    {noreply, reconnect(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    _ = disconnect(State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Just trigger a future reconnect, handled async.
reconnect(State) ->
    gen_server:cast(self(), reconnect),
    State.

%% Close the connection to Athena.
disconnect(#state{socket=undefined}=State) ->
    State;
disconnect(#state{socket=Socket}=State) ->
    _ = gen_tcp:close(Socket),
    Cleared = abort_inflight(State),
    Cleared#state{socket=undefined}.

abort_inflight(State) ->
    forward_response({error, <<"connection closed">>}, State),
    State#state{from=undefined, buffer=#buffer{}}.

%% Open the connection to Athena. Purposefully raises an exception if
%% a connection is already open.
connect(#state{socket=undefined, node={IP, Port}}=State) ->
    case catch gen_tcp:connect(IP, Port, [binary,{active,false}]) of
        {ok, Socket} ->
            Connected = State#state{socket=Socket},
            send_version(Connected);
        {_ErrorOrExit, Reason} when _ErrorOrExit == error;
                                    _ErrorOrExit == 'EXIT' ->
            log(error_msg, State,
                "Unable to connect to athena at (~p)", [Reason]),
            State
    end.

%% Send the client version. Begins an async wait for server version
%% response.
send_version(State) ->
    Msg = #athenarequest{
             protocol_request =
                 #protocolrequest{client_version = ?CLIENT_VERSION}},
    send(Msg, self_version, State).

%% Send an AthenaRequest to Athena.
send(#athenarequest{}=Msg, From, #state{socket=Socket}=State)
  when Socket /= undefined ->
    Encoded = athena_pb:encode(Msg),
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

new_request(#athenarequest{}=Msg, From,
            #state{from=undefined, socket=Socket}=State)
  when Socket /= undefined ->
    {noreply, send(Msg, From, State)};
new_request(#athenarequest{}=Msg, From, State) ->
    case queue_request(Msg, From, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, overload} ->
            {reply, {error, overload}, State}
    end.

queue_request(Msg, From, #state{queue=Q}=State) ->
    case queue:len(Q) >= ?QUEUE_LIMIT of
        false ->
            {ok, State#state{queue=queue:in({Msg, From}, Q)}};
        true ->
            {error, overload}
    end.

dequeue_request(#state{queue=Q}=State) ->
    case queue:out(Q) of
        {{value, {Msg, From}}, NewQ} ->
            send(Msg, From, State#state{queue=NewQ});
        {empty, _} ->
            State
    end.

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

find_response(Data, #buffer{}=Buffer) ->
    case find_length(Data, Buffer) of
        {ok, NewBuffer} ->
            try_decode(NewBuffer);
        {wait, NewBuffer} ->
            {wait, NewBuffer}
    end.

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

try_decode(#buffer{wait=Length, chunks=Chunks}=B) ->
    try_decode_result(B, get_bytes(Length, Chunks)).

try_decode_result(B, {ok, Bytes, NewChunks}) ->
    %% TODO: this iolist_to_binary is aggravating
    {ok, athena_pb:decode_athenaresponse(iolist_to_binary(Bytes)),
     B#buffer{wait=undefined, chunks=NewChunks}};
try_decode_result(B, {wait, NewChunks}) ->
    {wait, B#buffer{chunks=NewChunks}}.

forward_response(_Response, #state{from=undefined}=State) ->
    %% response is an error because the connection closed - nothing to see here
    State;
forward_response(Response, #state{from=self_version}=State) ->
    case Response of
        #athenaresponse{protocol_response=P} ->
            case P of
                #protocolresponse{server_version=V} when V /= undefined ->
                    log(info_msg, State, "Connected to server version ~p", [V]),
                    State#state{from=undefined};
                _ ->
                    log(error_msg, State, "Server did not reply with version"),
                    reconnect(State)
            end;
        _ ->
            log(error_msg, State, "Invalid server response"),
            reconnect(State)
    end;
forward_response(Response, #state{from=From}=State) ->
    gen_server:reply(From, Response),
    State#state{from=undefined}.

%% Implementation of a basic queue buffer. Chunks are added to
%% TailChunks, which remains in reverse order (head is most recently
%% received) until HeadChunks is empty, and which point it is reversed
%% and used as the new HeadChunks.
get_bytes(Count, {[], TailChunks}, NewChunk) ->
    get_bytes(Count, {lists:reverse([NewChunk|TailChunks]), []});
get_bytes(Count, {HeadChunks, TailChunks}, NewChunk) ->
    get_bytes(Count, {HeadChunks, [NewChunk|TailChunks]}).

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

log(Level, State, Message) ->
    log(Level, State, Message, []).
log(Level, #state{node=N}, Message, Params) ->
    error_logger:Level("~p: "++Message, [N|Params]).
