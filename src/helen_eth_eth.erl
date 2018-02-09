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

%% There is no mining in Athena.
-spec mining(#eth_request{}) -> {ok|error, mochijson2:json_term()}.
mining(_Request) ->
    {ok, false}.

-define(EMPTY_ADDRESS, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).
-define(EMPTY_VALUE, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).

%% Send a transaction.
%%
%% Omitting "to" implies contract creation. The generated address is
%% returned.
-spec sendTransaction(#eth_request{}) -> {ok|error, mochijson2:json_term()}.
sendTransaction(#eth_request{params=[{struct, Params}]}) ->
    case [get_param(<<"data">>, Params, <<>>),
          get_param(<<"from">>, Params, ?EMPTY_ADDRESS),
          get_param(<<"to">>, Params, ?EMPTY_ADDRESS),
          get_param(<<"value">>, Params, ?EMPTY_VALUE)] of
        [{ok, Data}, {ok, From}, {ok, To}, {ok, Value}] ->
            send_to_p2bc(Data, From, To, Value);
        Errors ->
            hd([ E || {error, _}=E <- Errors ])
    end;
sendTransaction(_)->
    {error, <<"Error: could not understand parameters">>}.

%% Send a raw transaction.
%%
%% The value passed as "data" should have the appropriate type byte on
%% the front already ("01" == create, "02" == call).
-spec sendRawTransaction(#eth_request{}) -> {ok|error, mochijson2:json_term()}.
sendRawTransaction(#eth_request{params=[{struct, Params}]}) ->
    case get_param(<<"data">>, Params, <<>>) of
        {ok, <<_P2BCType:1/binary, P2BCTo:20/binary, _/binary>>=Data} ->
            send_to_p2bc(P2BCTo, Data);
        {ok, _} ->
            {error, <<"Error: invalid 'data' parameter">>}
    end;
sendRawTransaction(_) ->
    {error, <<"Error: could not understand parameters">>}.

-spec get_param(binary(),
                [{mochijson2:json_term(), mochijson2:json_term()}],
                binary()) ->
          {ok|error, binary()}.
get_param(Name, Params, Default) ->
    case lists:keyfind(Name, 1, Params) of
        {Name, Value} ->
            case helen_eth:dehex(Value) of
                {ok, DHV} ->
                    {ok, ensure_length(DHV, Default)};
                Error ->
                    Error
            end;
        false ->
            {ok, Default}
    end.

%% this is probably only necessary for "value"
ensure_length(Value, Default) ->
    case {size(Value), size(Default)} of
        {SV, SD} when SV >= SD ->
            Value;
        {SV, SD} ->
            <<Default:(SD-SV)/binary, Value/binary>>
    end.

send_to_p2bc(Data, From, To, Value) ->
    {P2BCType,P2BCTo} = case To == ?EMPTY_ADDRESS of
                            true ->
                                {<<1>>, generate_address()}; %% create
                            false ->
                                {<<2>>, To}  %% call
                        end,

    P2BCMessage = <<P2BCType/binary, P2BCTo/binary, From/binary,
                    Value/binary, Data/binary>>,

    send_to_p2bc(P2BCTo, P2BCMessage).

-define(P2Clients, [
                    {{127,0,0,1}, 9204},
                    {{127,0,0,1}, 9205},
                    {{127,0,0,1}, 9206}
                   ]).

send_to_p2bc(P2BCTo, P2BCMessage) ->
    Results = [ send_to_p2bc_client(I, P, P2BCTo, P2BCMessage) ||
                  {I, P} <- ?P2Clients ],
    case lists:foldl(fun({R, V}, {Successes, Errors}) ->
                             case R of
                                 ok -> {[V|Successes], Errors};
                                 error -> {Successes, [V|Errors]}
                             end
                     end, {[], []}, Results) of
        {[H|_], []} ->
            {ok, H};
        {[], [H|_]} ->
            {error, H}
    end.

send_to_p2bc_client(P2ClientIP, P2ClientPort, P2BCTo, P2BCMessage) ->
    case gen_tcp:connect(P2ClientIP, P2ClientPort, [binary,{active,false}]) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, P2BCMessage) of
                ok ->
                    case gen_tcp:recv(Socket, 0, 2000) of
                        {ok, Reply} ->
                            {ok, helen_eth:hex0x(Reply)};
                        {error, closed} ->
                            {ok, helen_eth:hex0x(P2BCTo)};
                        {error, Reason} ->
                            {error, binary_reason("receive response", Reason)}
                    end;
                {error, Reason} ->
                    {error, binary_reason("send request", Reason)}
            end;
        {error, Reason} ->
            {error, binary_reason("connect", Reason)}
    end.

binary_reason(Msg, Reason) ->
    list_to_binary(io_lib:format("Error: failed to ~s (~p)", [Msg, Reason])).

generate_address() ->
    {ok, Digest} = helen_eth_web3:keccak_digest(term_to_binary(now())),
    <<Digest:20/binary>>.
