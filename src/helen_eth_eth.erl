%% Copyright 2018 VMware, all rights reserved.
%%
%% ETH RPC method handlers for eth_*.

-module(helen_eth_eth).

-export([
         mining/1,
         sendTransaction/1,
         getTransactionReceipt/1
        ]).

-include("helen_eth.hrl").
-include("athena_pb.hrl").

-import(helen_eth_param, [
                          optional_0x/2, required_0x/2,
                          optional_address/2, required_address/2,
                          optional_integer/2
                         ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Method handlers

%% There is no mining in Athena.
-spec mining(#eth_request{}) -> {ok|error, mochijson2:json_term()}.
mining(_Request) ->
    {ok, false}.

%% Send a transaction.
%%
%% Omitting "to" implies contract creation. The generated address is
%% returned.
-spec sendTransaction(#eth_request{}) -> {ok|error, mochijson2:json_term()}.
sendTransaction(#eth_request{params=[{struct, Params}]}) ->
    case [optional_address(<<"to">>, Params),
          required_address(<<"from">>, Params),
          optional_integer(<<"value">>, Params),
          optional_0x(<<"data">>, Params)] of
        [{ok, To}, {ok, From}, {ok, Value}, {ok, Data}] ->

            sendTransaction_athena(To, From, Value, Data);
        Errors ->
            hd([ E || {error, _}=E <- Errors ])
    end;
sendTransaction(_)->
    {error, <<"Could not understand parameters">>}.

%% Fetch a transaction receipt
-spec getTransactionReceipt(#eth_request{}) ->
         {ok|error, mochijson2:json_term()}.
getTransactionReceipt(#eth_request{params=[Receipt]}) ->
    case helen_eth:dehex(Receipt) of
        {ok, TxHash} ->
            getTransactionReceipt_athena(TxHash);
        _ ->
            {error, <<"Invalid transaction hash">>}
    end;
getTransactionReceipt(_) ->
    {error, <<"Could not understand parameters">>}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Athena implementation

sendTransaction_athena(To, From, Value, Data) ->
    EthRequest = #ethrequest{
                    method= 'SEND_TX',
                    addr_to= case To of <<>> -> undefined; _ -> To end,
                    addr_from=From,
                    value= case Value of <<>> -> undefined; _ -> Value end,
                    data= Data},
    case helen_athena_conn:send_request(
           #athenarequest{eth_request=[EthRequest]}) of
        #athenaresponse{eth_response=[EthResponse]} ->
            error_logger:info_msg("received response from athena: ~p",
                                  [EthResponse]),
            case EthResponse of
                #ethresponse{data=TxHash} when TxHash /= undefined ->
                    {ok, helen_eth:hex0x(TxHash)};
                _ ->
                    {error, <<"bad response from athena">>}
            end;
        Other ->
            error_logger:error_msg("did not understand athena response: !p",
                                   [Other]),
            {error, <<"bad response from athena">>}
    end.

getTransactionReceipt_athena(TxHash) ->
    EthRequest = #ethrequest{
                    method= 'GET_TX_RECEIPT',
                    data= TxHash},
    case helen_athena_conn:send_request(
           #athenarequest{eth_request=[EthRequest]}) of
        #athenaresponse{eth_response=[EthResponse]} ->
            error_logger:info_msg("received response from athena: ~p",
                                  [EthResponse]),
            case EthResponse of
                #ethresponse{status=Status,
                             contract_address=Address} ->
                    {ok, {struct, [{<<"status">>, Status},
                                   {<<"transactionHash">>,
                                    helen_eth:hex0x(TxHash)}
                                   |[{<<"contractAddress">>,
                                      helen_eth:hex0x(Address)}
                                     || Address /= undefined]]}};
                _ ->
                    {error, <<"bad response from athena">>}
            end;
        Other ->
            error_logger:error_msg("did not understand athena response: !p",
                                   [Other]),
            {error, <<"bad response from athena">>}
    end.
