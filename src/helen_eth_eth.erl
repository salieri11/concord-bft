%% Copyright 2018 VMware, all rights reserved.
%%
%% ETH RPC method handlers for eth_*.

-module(helen_eth_eth).

-export([
         mining/1,
         sendTransaction/1,
         sendRawTransaction/1
        ]).

-include("helen_eth.hrl").

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
            sendTransaction_P2BC(To, From, Value, Data);
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
        {ok, <<_P2BCType:1/binary, P2BCTo:20/binary, _/binary>>=Data} ->
            sendRawTransaction_P2BC(P2BCTo, Data);
        {ok, _} ->
            {error, <<"Invalid 'data' parameter">>}
    end;
sendRawTransaction(_) ->
    {error, <<"Could not understand parameters">>}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

binary_reason(Msg, Reason) ->
    list_to_binary(io_lib:format("failed to ~s (~p)", [Msg, Reason])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% P2_Blockchain implementation

%% Build a transaction command and send it to a P2_Blockchain cluster.
sendTransaction_P2BC(To, From, Value, Data) ->
    {P2BCType,P2BCTo} = case To == <<>> of
                            true ->
                                %% create
                                {<<1>>, generate_address()};
                            false ->
                                %% call
                                {<<2>>, To}  %% call
                        end,

    sendRawTransaction_P2BC(P2BCTo,
                            <<P2BCType/binary, P2BCTo/binary,
                              From/binary, Value/binary, Data/binary>>).

%% Send a pre-built transaction to a P2_Blockchain cluster.
sendRawTransaction_P2BC(To, Message) ->
    Results = [ sendRawTransaction_P2BC_client(C, To, Message)
                || C <- clients_P2BC() ],
    case lists:partition(fun({R, _}) -> R == ok end, Results) of
        {[{ok, _}=Success|_], []} ->
            Success;
        {_, [{error, _}=Error|_]} ->
            Error;
        {[], []} ->
            {error, "No clients defined."}
    end.

%% Send a pre-built transaction to one P2_Blockchain Blockchain_client.
sendRawTransaction_P2BC_client({IP, Port}, To, Message) ->
    case catch gen_tcp:connect(IP, Port, [binary,{active,false}]) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, Message) of
                ok ->
                    case gen_tcp:recv(Socket, 0, 2000) of
                        {ok, Reply} ->
                            {ok, helen_eth:hex0x(Reply)};
                        {error, closed} ->
                            {ok, helen_eth:hex0x(To)};
                        {error, Reason} ->
                            {error, binary_reason("receive response", Reason)}
                    end;
                {error, Reason} ->
                    {error, binary_reason("send request", Reason)}
            end;
        {error, Reason} ->
            {error, binary_reason("connect", Reason)};
        {'EXIT', Reason} ->
            {error, binary_reason("connect", Reason)}
    end.

%% Get a list of clients in the P2_Blockchain cluster
-spec clients_P2BC() -> [{inet:ip_address(), inet:port_number()}].
clients_P2BC() ->
    case application:get_env(helen, p2bc_clients) of
        {ok, Clients} ->
            Clients;
        undefined ->
            []
    end.

%% Generate an address for contract creation.
generate_address() ->
    %% TODO: term_to_binary/1 and now/0 are both slow
    %% TODO: we don't need a full digest here
    {ok, Digest} = helen_eth_web3:keccak_digest(term_to_binary(now())),
    <<Digest:20/binary>>.
