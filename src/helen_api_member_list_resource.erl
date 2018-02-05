%% Copyright 2018 VMware, all rights reserved.
%%
%% List members in this Athena cluster. Includes only Athena nodes,
%% not Helen nodes.

-module(helen_api_member_list_resource).

-export([
         init/1,
         content_types_provided/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init([]) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

-spec content_types_provided(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

-spec to_json(wrq:reqdata(), term()) ->
          {{halt, integer()}|iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    %% mocking this for UI testing
    FakeMemberList = [
                      {struct, [
                                {<<"host">>, <<"athena1">>},
                                {<<"status">>, <<"connected">>}
                               ]},
                      {struct, [
                                {<<"host">>, <<"athena2">>},
                                {<<"status">>, <<"connected">>}
                               ]},
                      {struct, [
                                {<<"host">>, <<"athena3">>},
                                {<<"status">>, <<"unavailable">>}
                               ]}
                     ],
    {mochijson2:encode(FakeMemberList), ReqData, State}.
