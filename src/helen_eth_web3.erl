%% Copyright 2018 VMware, all rights reserved.
%%
%% Implementation of web3_* Ethereum JSON RPC. Requires crypto++
%% library is installed to compute Keccak-256.

-module(helen_eth_web3).

-export([
         clientVersion/1,
         sha3/1,
         load_nif/0
        ]).

-include("helen_eth.hrl").

-onload(load_nif/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RPC Methods

-spec clientVersion(#eth_request{}) -> iodata().
clientVersion(_Request) ->
    <<"Helen/1.0.0">>.

-spec sha3(#eth_request{}) -> iodata().
sha3(#eth_request{params=[String0x]}) ->
    case dehex(String0x) of
        {ok, Bin} ->
            case catch keccak_digest(Bin) of
                Digest when is_binary(Digest) ->
                    hex0x(Digest);
                _ ->
                    <<"Failed to compute digest">>
            end;
        {error, Reason} ->
            Reason
    end;
sha3(_Requestn) ->
    <<"ERROR: wrong number of parameters">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Support functions

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

%% Convert a binary to "0x<data>" format
-spec hex0x(binary()) -> binary().
hex0x(Binary) ->
    Hex = << <<(hexval(H)), (hexval(L))>> || <<H:4, L:4>> <= Binary >>,
    << $0, $x, Hex/binary >>.

%% Convert a nibble to its hexadecimal numeral.
-spec hexval(byte()) -> byte().
hexval(X) when X < 10 ->
    X + $0;
hexval(X) ->
    X + $a - 10.

%% Calculate the digest - see NIF definition in helen_eth_web3.cpp.
-spec keccak_digest(binary()) -> binary().
keccak_digest(_Bin) ->
    <<"ERROR: web3 nif not loaded">>.

load_nif() ->
    Path = filename:join([code:priv_dir(helen), "helen_eth_web3"]),
    erlang:load_nif(Path, 0).
