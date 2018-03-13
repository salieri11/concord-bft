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
    Priv = code:priv_dir(helen),
    lists:flatten([
        {["api"], helen_api_base_resource, []},
        {["api", "athena", "members"], helen_api_member_list_resource, []},
        {["api", "athena", "blocks"], helen_api_block_list_resource, []},
        {["api", "athena", "blocks", block_id], helen_api_block_resource, []},
        {["api", "athena", "eth"], helen_api_eth_resource, []},
        %% enforce that we don't serve the Angular HTML at undefined /api URLs
        {["api", '*'], helen_404_resource, []},

        {["swagger", '*'], helen_static_resource,
         [{dir, filename:join([Priv, "www", "swagger"])}]},
        {["assets", '*'], helen_static_resource,
         [{dir, filename:join([Priv, "www", "assets"])}]},

        %% Everything else will load the single-page Angular app. It
        %% needs every URL to serve its base HTML. It decides which
        %% page to render, on the browser side, based on the URL.
        {['*'], helen_static_resource,
         [{file, filename:join([Priv, "www", "index.html"])}]}
    ]).

web_config() ->
    {ok, Ip} = application:get_env(helen, web_ip),
    {ok, Port} = application:get_env(helen, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch()}
    ].
