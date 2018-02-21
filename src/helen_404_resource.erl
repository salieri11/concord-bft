%% Copyright 2018 VMware, all rights reserved.
%%
%% Resource to reserve URL space by forcing 404 status.
%%
%% The UI is served by angular, which requires us to serve a static
%% HTML file from basically every URL that we are not serving
%% something else from. Especially for URLs under /api, it's better to
%% serve a 404 than a 200 with irrelevant HTML. This resource allows
%% us to easily specify paths that *should* 404 instead of returning
%% Angular HTML.

-module(helen_404_resource).
-export([
         init/1,
         resource_exists/2
]).

-include_lib("webmachine/include/webmachine.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init([{dir,string()}]) -> {ok, undefined}.
init(Props) ->
    {ok, undefined}.

-spec resource_exists(wrq:reqdata(), undefined) ->
          {boolean(), wrq:reqdata(), undefined}.
resource_exists(ReqData, State) ->
    {false, ReqData, State}.

%% TODO: consider a JSON-formatted 404 message under /api?
