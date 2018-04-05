%% Copyright 2018 VMware, all rights reserved.
%%
%% Utilities for supporting the ETH JSON RPC API.

-module(helen_eth).

-export([
         dehex_bytes/1,
         dehex_quantity/1,
         hex0x/1
        ]).

%% Convert a "0x<data>" string to its binary value.
-spec dehex_bytes(binary()) -> {ok, iodata()} | {error, iodata()}.
dehex_bytes(<<$0:8, $x:8, String/binary>>) ->
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
dehex_bytes(_) ->
    {error, <<"ERROR: invalid data format (missing \"0x\")">>}.

%% Convert a "0x<data>" string to its numeric value.
-spec dehex_quantity(binary()) -> {ok, integer()} | {error, iodata()}.
dehex_quantity(<<$0:8, $x:8>>) ->
    {ok, 0};
dehex_quantity(<<$0:8, $x:8, String/binary>>) ->
    case catch list_to_integer(binary_to_list(String), 16) of
        {'EXIT', _} ->
            {error, <<"ERROR: invalid character in data">>};
        Number ->
            {ok, Number}
    end;
dehex_quantity(_) ->
    {error, <<"ERROR: invalid data format (missing \"0x\")">>}.

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
