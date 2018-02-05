%% Copyright 2018 VMware, all rights reserved.
%%
%% Dispatch and application configuration.

-module(helen_config).

-export([
    dispatch/0,
    web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
        {["api"], helen_api_base_resource, []},
        {["api", "eth"], helen_api_eth_resource, []},
        %% serve everything in priv/www, at URLs matching the paths
        {['*'], helen_static_resource, []}
    ]).

web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch()}
    ].
