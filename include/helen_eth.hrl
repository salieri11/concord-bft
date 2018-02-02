%% Copyright 2018 VMware, all rights reserved.
%%
%% Common definitions for Ethereum JSON RPC interface.

-record(eth_request, {
          method :: binary(),              %% Method name
          id :: integer(),                 %% Request id
          params :: mochijson2:json_term() %% Parameters
}).
