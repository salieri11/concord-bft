%% Copyright 2018 VMware, all rights reserved.
%%
%% Resource for getting a specific transaction

-module(helen_api_transaction_resource).

-export([
         init/1,
         malformed_request/2,
         content_types_provided/2,
         resource_exists/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("athena_pb.hrl").

-record(state, {
          tx_hash :: malformed | binary(),
          tx
         }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init([]) -> {ok|{trace,string()}, term()}.
init([]) ->
    {ok, #state{}}.

-spec malformed_request(wrq:reqdata(), #state{}) ->
          {boolean()|{halt, integer()}, wrq:reqdata(), #state{}}.
malformed_request(ReqData, State) ->
    case choose_tx(ReqData, State) of
        #state{tx_hash=malformed}=NewState ->
            {true, ReqData, NewState};
        #state{tx_hash=TxHash}=NewState ->
            case TxHash of
                <<$0, $x, _:64/binary>> ->
                    %% good hash
                    {false, ReqData, NewState};
                _ ->
                    %% bad hash
                    {true, ReqData, NewState}
            end
    end.

-spec content_types_provided(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

-spec resource_exists(wrq:reqdata(), term()) ->
          {boolean()|{halt, integer()}, wrq:reqdata(), #state{}}.
resource_exists(ReqData, State) ->
    case get_tx(choose_tx(ReqData, State)) of
        #state{tx=not_found}=NewState ->
            {false, ReqData, NewState};
        #state{tx=error}=NewState ->
            {{halt, 500}, ReqData, NewState};
        NewState ->
            {true, ReqData, NewState}
    end.

-spec to_json(wrq:reqdata(), term()) ->
          {{halt, integer()}|iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    case get_tx(choose_tx(ReqData, State)) of
        #state{tx=Tx}=State when is_tuple(Tx) ->
            {mochijson2:encode(Tx), ReqData, State};
        _ ->
            Response = {struct, [{<<"error">>,
                                  <<"invalid response from server">>}]},
            {{halt, 500},
             wrq:set_resp_body(mochijson2:encode(Response), ReqData),
             State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal

choose_tx(ReqData, #state{tx_hash=undefined}=State) ->
    case orddict:find(tx_hash, wrq:path_info(ReqData)) of
        {ok, TxHash} ->
            #state{tx_hash=list_to_binary(TxHash)};
        error ->
            %% this shouldn't happen - block_id is set by webmachine
            %% dispatch, but just in case we mess up the dispatch
            %% table...
            State#state{tx_hash=malformed}
    end;
choose_tx(_ReqData, State) ->
    %% memoized
    State.

get_tx(#state{tx_hash=TxHash, tx=undefined}=State) when is_binary(TxHash) ->
    %% TODO: Ask Athena
    %% No, this is not how hashes work, but it gives the mock texture
    <<$0, $x, BlockNumber/integer, From/integer, To/integer,
      Value:16/integer, TxIndex/integer, Input/binary>> = TxHash,
    {ok, Block} = helen_eth_web3:keccak_digest(<<BlockNumber/integer>>),
    {ok, <<FromHash:20/binary, _/binary>>} =
        helen_eth_web3:keccak_digest(<<From/integer>>),
    {ok, <<ToHash:20/binary, _/binary>>} =
        helen_eth_web3:keccak_digest(<<To/integer>>),
    Tx = {struct, [
                   {<<"hash">>, TxHash},
                   {<<"from">>, helen_eth:hex0x(FromHash)},
                   {<<"to">>, helen_eth:hex0x(ToHash)},
                   {<<"value">>, Value},
                   {<<"input">>, helen_eth:hex0x(Input)},
                   {<<"blockHash">>, helen_eth:hex0x(Block)},
                   {<<"blockNumber">>, BlockNumber},
                   {<<"transactionIndex">>, TxIndex},
                   {<<"nonce">>, <<$0, $x, Input/binary>>}
                  ]},
    State#state{tx=Tx};
get_tx(#state{tx=undefined}=State) ->
    %% malformed tx hash
    State#state{tx=malformed};
get_tx(State) ->
    %% memoized
    State.
