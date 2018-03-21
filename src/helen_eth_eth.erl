%% Copyright 2018 VMware, all rights reserved.
%%
%% ETH RPC method handlers for eth_*.

-module(helen_eth_eth).

-export([
         mining/1,
         sendTransaction/1,
         sendRawTransaction/1
        ]).
-export([
         sendTransaction_athena/4
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

%% Send a raw transaction.
%%
%% The value passed as "data" should have the appropriate type byte on
%% the front already ("01" == create, "02" == call).
-spec sendRawTransaction(#eth_request{}) -> {ok|error, mochijson2:json_term()}.
sendRawTransaction(#eth_request{params=[{struct, Params}]}) ->
    case required_0x(<<"data">>, Params) of
        {ok, Data} ->
            sendRawTransaction_athena(Data);
        _ ->
            {error, <<"Invalid 'data' parameter">>}
    end;
sendRawTransaction(_) ->
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
            {ok, <<"todosomething">>};
        Other ->
            error_logger:error_msg("did not understand athena response: !p",
                                   [Other]),
            {error, <<"bad response from athena">>}
    end.

sendRawTransaction_athena(Data) ->
    EthRequest = #ethrequest{method='SEND_TX_RAW', data=Data},
    case helen_athena_conn:send_request(
           #athenarequest{eth_request=[EthRequest]}) of
        #athenaresponse{eth_response=[EthResponse]} ->
            error_logger:info_msg("received response from athena: ~p",
                                  [EthResponse]),
            {ok, <<"todorawsomething">>};
        Other ->
            error_logger:error_msg("did not understand athena response: !p",
                                   [Other]),
            {error, <<"bad response from athena">>}
    end.
