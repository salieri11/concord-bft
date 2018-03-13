%% Copyright 2018 VMware, all rights reserved.
%%
%% Resource for getting a specific block

-module(helen_api_block_resource).

-export([
         init/1,
         malformed_request/2,
         content_types_provided/2,
         resource_exists/2,
         to_json/2,
         url/1
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("athena_pb.hrl").

-record(state, {
          block_id :: malformed | integer(),
          block
         }).

%% Number of transactions to mock in a block
-define(TRANSACTION_COUNT, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init([]) -> {ok|{trace,string()}, term()}.
init([]) ->
    {ok, #state{}}.

-spec malformed_request(wrq:reqdata(), #state{}) ->
          {boolean()|{halt, integer()}, wrq:reqdata(), #state{}}.
malformed_request(ReqData, State) ->
    case choose_block(ReqData, State) of
        #state{block_id=malformed}=NewState ->
            {true, ReqData, NewState};
        #state{}=NewState ->
            {false, ReqData, NewState}
    end.

-spec content_types_provided(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

-spec resource_exists(wrq:reqdata(), term()) ->
          {boolean()|{halt, integer()}, wrq:reqdata(), #state{}}.
resource_exists(ReqData, State) ->
    case get_block(choose_block(ReqData, State)) of
        #state{block=not_found}=NewState ->
            {false, ReqData, NewState};
        #state{block=error}=NewState ->
            {{halt, 500}, ReqData, NewState};
        NewState ->
            {true, ReqData, NewState}
    end.

-spec to_json(wrq:reqdata(), term()) ->
          {{halt, integer()}|iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    case get_block(choose_block(ReqData, State)) of
        #state{block=Block}=State when is_tuple(Block) ->
            {mochijson2:encode(Block), ReqData, State};
        _ ->
            Response = {struct, [{<<"error">>,
                                  <<"invalid response from server">>}]},
            {{halt, 500},
             wrq:set_resp_body(mochijson2:encode(Response), ReqData),
             State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal

url(N) ->
    iolist_to_binary([<<"/api/athena/blocks/">>, integer_to_list(N)]).

choose_block(ReqData, #state{block_id=undefined}=State) ->
    case orddict:find(block_id, wrq:path_info(ReqData)) of
        {ok, BlockIdString} ->
            case catch list_to_integer(BlockIdString) of
                {'EXIT', _} ->
                    %% TODO: support block hash
                    State#state{block_id=malformed};
                BlockId ->
                    State#state{block_id=BlockId}
            end;
        error ->
            %% this shouldn't happen - block_id is set by webmachine
            %% dispatch, but just in case we mess up the dispatch
            %% table...
            State#state{block_id=malformed}
    end;
choose_block(_ReqData, State) ->
    %% memoized
    State.

%% based on reply to eth_getBlockByHash
%% https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_getblockbyhash
get_block(#state{block_id=BlockId, block=undefined}=State) when is_integer(BlockId) ->
    %% TODO: Ask Athena
    {ok, Digest} = helen_eth_web3:keccak_digest(<<BlockId/integer>>),
    {ok, Parent} = helen_eth_web3:keccak_digest(<<(BlockId-1)/integer>>),
    Block = {struct, [
                      {<<"number">>, BlockId},
                      {<<"hash">>, helen_eth:hex0x(Digest)},
                      {<<"parentHash">>, helen_eth:hex0x(Parent)},
                      {<<"nonce">>, helen_eth:hex0x(Digest)},
                      {<<"size">>, BlockId*100},
                      {<<"transactions">>,
                       [ fake_transaction(BlockId, N)
                         || N <- lists:seq(1, ?TRANSACTION_COUNT) ]}
                     ]},
    State#state{block=Block};
get_block(#state{block=undefined}=State) ->
    %% invalid block id
    State#state{block=error};
get_block(State) ->
    %% memoized
    State.

fake_transaction(BlockId, N) ->
    {ok, Digest} = helen_eth_web3:keccak_digest(<<(BlockId*1000+N)/integer>>),
    helen_eth:hex0x(Digest).
