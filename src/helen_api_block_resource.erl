%% Copyright 2018 VMware, all rights reserved.
%%
%% Resource for getting a specific block

-module(helen_api_block_resource).

-export([
         url/1
        ]).

url(N) ->
    iolist_to_binary([<<"/api/athena/blocks/">>, integer_to_list(N)]).
