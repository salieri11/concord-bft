%% Copyright 2018 VMware, all rights reserved.
%%
%% The base API resource for Helen. This will serve the swagger
%% definition, to point apps toward the rest of the API.
%%
%% TODO: we can stick a long expiry time on here

-module(helen_api_base_resource).

-export([
         init/1,
         content_types_provided/2,
         to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

-spec content_types_provided(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

-spec to_json(wrq:reqdata(), term()) ->
          {{halt, integer()}|iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    case code:priv_dir(helen) of
        {error, bad_name} ->
            {{halt, 500}, ReqData, State};
        Filename ->
            %% TODO: we can preload this when the app starts, instead
            %% of reading every time
            SwaggerFile = filename:join([Filename, "swagger.json"]),
            case file:read_file(SwaggerFile) of
                {ok, IoData} ->
                    {IoData, ReqData, State};
                {error, _Reason} ->
                    {{halt, 500}, ReqData, State}
            end
    end.
