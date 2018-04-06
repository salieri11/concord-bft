%% Copyright 2018 VMware, all rights reserved.
%%
%% Test the utility functions

-module(helen_eth_test).

%% eunit includes -comple(export_all).
-include_lib("eunit/include/eunit.hrl").

%% An zero-length piece of data is encoded as just "0x"
empty_binary_test() ->
    ?assertEqual(<<"0x">>, helen_eth:hex0x(<<>>)).

%% "0x" encodes a zero-length piece of data
empty_string0x_test() ->
    ?assertEqual({ok, <<>>}, helen_eth:dehex_bytes(<<"0x">>)).

%% Run ten random dehex tests
dehex_test_() ->
    dehex_test_gen(10).

dehex_test_gen(Tests) ->
    {generator,
     fun() ->
             [dehex_test_iter(generate_string0x())
              |[dehex_test_gen(Tests-1) || Tests > 0]]
     end}.

%% Test that dehex works by testing a round-trip: given a 0x-encoded
%% string, dehex it, then check that re-0x-encoding it gives the
%% original result.
dehex_test_iter(String0x) ->
    ?_assertEqual(String0x,
                  helen_eth:hex0x(
                    element(2, {ok,_}=helen_eth:dehex_bytes(String0x)))).

%% Run ten random hex0x tests
hex0x_test_() ->
    hex0x_test_gen(10).

hex0x_test_gen(Tests) ->
    {generator,
     fun() ->
             [hex0x_test_iter(generate_binary())
              |[hex0x_test_gen(Tests-1) || Tests > 0]]
     end}.

%% Test that hex0x works by testing a round-trip: generate a random
%% binary, 0x-encoded it, then check that re-decoding it gives the
%% original result.
hex0x_test_iter(Binary) ->
    ?_assertEqual({ok, Binary}, helen_eth:dehex_bytes(helen_eth:hex0x(Binary))).

%% Check the invalid inputs produce errors, and not accidental 'ok's.
dehex_invalid_test_() ->
    [
     fun() -> {error, _} = helen_eth:dehex_bytes(<<"1234">>) end,  % no "0x"
     fun() -> {error, _} = helen_eth:dehex_bytes(<<"0x123">>) end, % odd nibbles
     fun() -> {error, _} = helen_eth:dehex_bytes(<<"0xgh">>) end   % non hex chars
    ].

%% Generate a random binary, up to 1024 bytes in length.
generate_binary() ->
    list_to_binary([ rand:uniform(256)-1 ||
                       _ <- lists:seq(1, rand:uniform(1024)) ]).

%% Generate a random 0x-encoded string, up to 1026 (including the
%% "0x") characters in length.
generate_string0x() ->
    Bin = list_to_binary([ io_lib:format("~2.16.0b", [rand:uniform(256)-1])
                           || _ <- lists:seq(1, rand:uniform(1024)) ]),
    << $0, $x, Bin/binary >>.
