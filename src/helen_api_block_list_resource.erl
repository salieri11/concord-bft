%% Copyright 2018 VMware, all rights reserved.
%%
%% List blocks that have been committed to the chain.

-module(helen_api_block_list_resource).

-export([
         init/1,
         malformed_request/2,
         content_types_provided/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("athena_pb.hrl").

%% Number of blocks to return in a request
-define(BLOCK_COUNT, 10).

-define(FAKE_LATEST_BLOCK_NUMBER, 100).

-record(state, {
          start :: malformed | error | integer() %% latest block to list
         }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init([]) -> {ok|{trace,string()}, term()}.
init([]) ->
    {ok, #state{}}.

-spec malformed_request(wrq:reqdata(), #state{}) ->
          {boolean()|{halt, integer()}, wrq:reqdata(), #state{}}.
malformed_request(ReqData, State) ->
    case choose_start(ReqData, State) of
        #state{start=malformed}=NewState ->
            %% "start" parameter was given, and invalid
            {true, ReqData, NewState};
        #state{start=error}=NewState ->
            %% "start" parameter was not given, and we had trouble
            %% getting the most recent block number
            {{halt, 500}, ReqData, NewState};
        NewState ->
            %% either proper start was given, or we were able to choose one
            {false, ReqData, NewState}
    end.

-spec content_types_provided(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

-spec to_json(wrq:reqdata(), term()) ->
          {{halt, integer()}|iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    case get_blocks(choose_start(ReqData, State)) of
        {ok, Blocks} ->
            Response = {struct, [{<<"blocks">>, Blocks},
                                 {<<"next">>, next_url(Blocks)}]},
            {mochijson2:encode(Response), ReqData, State};
        _ ->
            Response = {struct, [{<<"error">>,
                                  <<"invalid response from server">>}]},
            {{halt, 500},
             wrq:set_resp_body(mochijson2:encode(Response), ReqData),
             State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal

choose_start(ReqData, #state{start=undefined}=State) ->
    case wrq:get_qs_value("start", ReqData) of
        undefined ->
            State#state{start=get_last_block_number()};
        QueryStart ->
            case catch list_to_integer(QueryStart) of
                {'EXIT', _} ->
                    State#state{start=malformed};
                Start ->
                    State#state{start=Start}
            end
    end;
choose_start(_ReqData, State) ->
    %% memoized
    State.

get_last_block_number() ->
    %% TODO: ask Athena
    ?FAKE_LATEST_BLOCK_NUMBER.

get_blocks(#state{start=Start}) ->
    case now() of
        {1520,920134,46774} ->
            {error, <<"appease dialyzer">>};
        _ ->
            %% TODO: ask Athena
            {ok, [ fake_block(N)
                   || N <- lists:seq(Start, max(Start-?BLOCK_COUNT+1, 0), -1) ]}
    end.

fake_block(N) ->
    {ok, Digest} = helen_eth_web3:keccak_digest(<<N/integer>>),
    {struct, [{<<"number">>, N},
              {<<"hash">>, helen_eth:hex0x(Digest)},
              {<<"url">>, helen_api_block_resource:url(N)}]}.

next_url(Blocks) ->
    {struct, Props} = lists:last(Blocks),
    {<<"number">>, N} = lists:keyfind(<<"number">>, 1, Props),
    Next = max(?BLOCK_COUNT-1, N-1),
    iolist_to_binary([<<"/api/athena/blocks?start=">>, integer_to_list(Next)]).
