%% Copyright 2018 VMware, all rights reserved.
%%
%% Test the web3 rpc definitions.

-module(helen_eth_web3_test).

-export([
         sha3_test_/0
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("helen_eth.hrl").

-define(EXAMPLE_HASHES,
   [
    {<<"0x666f6f626172">>, %% "foobar"
     <<"0x38d18acb67d25c8bb9942764b62f18e17054f66a817bd4295423adf9ed98873e">>},
    {<<"0x666f6f62617231">>, %% "foobar1"
     <<"0x94b20ebc895ddd6ed11f1c4334cd3922ea1c531867c97b0d3efd3f52031df6e4">>},
    {<<"0x666f6f62617232">>, %% "foobar2"
     <<"0xc99828f2edcfc11bc085a82290f68f4978ba9a22321bf9de94d12625e3e90ad7">>},
    {<<"0x666f6f62617233">>, %% "foobar3"
     <<"0x814e46f3f371dbc3a6024e4599e2ac9000d206f1f1d62eea9ebad331bfa052f7">>}
   ]).

%% There isn't really a second Keccak implementation to test against,
%% so for now, this is just a few static tests taken from an official
%% Ethereum session.
sha3_test_() ->
    {setup,
     fun() -> helen_eth_web3:load_nif() end,
     [ ?_assertEqual({ok, Hash}, sha3_test_helper(Input)) ||
         {Input, Hash} <- ?EXAMPLE_HASHES ]}.

sha3_test_helper(Input) ->
    helen_eth_web3:sha3(#eth_request{id = 1,
                                     method = <<"web3_sha3">>,
                                     params = [Input]}).
