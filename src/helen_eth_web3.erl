%% Copyright 2018 VMware, all rights reserved.
%%
%% Implementation of web3_* Ethereum JSON RPC. Requires crypto++
%% library is installed to compute Keccak-256.

-module(helen_eth_web3).

-export([
         clientVersion/1,
         sha3/1
        ]).

-include("helen_eth.hrl").

-spec clientVersion(#eth_request{}) -> iodata().
clientVersion(_Request) ->
    <<"Helen/1.0.0">>.

-spec sha3(#eth_request{}) -> iodata().
sha3(#eth_request{params=[String0x]}) ->
    case dehex(String0x) of
        {ok, Bin} ->
            [<<"TODO: hash of ">>, size(Bin), <<" bytes">>];
        {error, Reason} ->
            Reason
    end;
sha3(_Requestn) ->
    <<"ERROR: wrong number of parameters">>.

%% Convert a "0x<data>" string to its binary value.
-spec dehex(binary()) -> {ok, iodata()} | {error, iodata()}.
dehex(<<$0:8, $x:8, String/binary>>) ->
    case size(String) rem 2 of
        0 ->
            case catch << <<((binval(A) bsl 4) bor binval(B)):8>>
                          || <<A:8, B:8>> <= String >> of
                {'EXIT', _} ->
                    {error, <<"ERROR: invalid character in data">>};
                Data ->
                    {ok, Data}
            end;
        1 ->
            {error, <<"ERROR: invalid data length (odd nibble count)">>}
    end;
dehex(_) ->
    {error, <<"ERROR: invalid data format (missing \"0x\"">>}.

%% Convert a hex digit to its integer value
-spec binval(byte()) -> integer().
binval(L) when L >= $0, L =< $9 ->
    L - $0;
binval(L) when L >= $a, L =< $f ->
    L - $a + 10;
binval(L) when L >= $A, L =< $F ->
    L - $A + 10.
