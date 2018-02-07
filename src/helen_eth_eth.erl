%% Copyright 2018 VMware, all rights reserved.
%%
%% ETH RPC method handlers for eth_*.

-module(helen_eth_eth).

-export([
         mining/1
        ]).

%% There is no mining in Athena.
mining(_Request) ->
    false.
