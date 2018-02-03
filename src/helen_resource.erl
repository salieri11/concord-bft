%% Copyright 2018 VMware, all rights reserved.
%%
%% The base resource for Helen, the Project Athena UI/API. This
%% resource will eventually serve the landing page for the UI.

-module(helen_resource).
-export([
         init/1,
         to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
    NewReqData = wrq:set_resp_header("Link", "</api>; rel=api", ReqData),
    {"<html>"
     "<head><title>VMware Project Athena UI</title></head>"
     "<body><h1>VMware Project Athena UI (TODO)</h1>"
     "<p>You're probably looking for the "
     "<a href=\"/swagger/\">API Documentation</a> for now.</p>"
     "</body></html>",
     NewReqData, State}.
