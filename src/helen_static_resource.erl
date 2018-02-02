%% Copyright 2018 VMware, all rights reserved.
%%
%% Static file serving resource. Serves files from /priv/www, unless a
%% "dir" property in the init list points elsewhere.

-module(helen_static_resource).
-export([
         init/1,
         resource_exists/2,
         content_types_provided/2,
         forbidden/2,
         produce_content/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {
          dir :: string(),
          clean_path :: string()
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init([{dir,string()}]) -> {ok, #state{}}.
init(Props) ->
    Dir = case lists:keyfind(dir, 1, Props) of
              {dir, D} -> D;
              false -> filename:join([code:priv_dir(helen), "www"])
          end,
    {ok, #state{dir=Dir}}.

-spec resource_exists(wrq:reqdata(), #state{}) ->
          {boolean(), wrq:reqdata(), #state{}}.
resource_exists(ReqData, State) ->
    #state{clean_path=Path}=NewState = clean_path(ReqData, State),
    %% is_file == "exists", not "is regular file"
    {filelib:is_file(Path), ReqData, NewState}.

-spec content_types_provided(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    #state{clean_path=Path}=NewState = clean_path(ReqData, State),
    %% provide the guessed type of the file only
    {[{webmachine_util:guess_mime(Path), produce_content}],
     ReqData, NewState}.

-define(NO_LISTING,
        <<"<html><head><title>403 Forbidden</title></head>",
          "<body><h1>403 Forbidden</h1>",
          "<p>Directory listing is forbidden</p></body></html>">>).

-spec forbidden(wrq:reqdata(), #state{}) ->
          {boolean(), wrq:reqdata(), #state{}}.
forbidden(ReqData, State) ->
    #state{clean_path=Path}=State1 = clean_path(ReqData, State),
    {Forbidden, NewReqData, NewState} =
        case filelib:is_dir(Path) of
            true ->
                IndexPath = filename:join([Path, "index.html"]),
                case filelib:is_regular(IndexPath) of
                    true ->
                        {false, ReqData, State1#state{clean_path=IndexPath}};
                    false ->
                        %% listing of directories is not supported
                        {true, wrq:set_resp_body(?NO_LISTING, ReqData), State1}
                end;
            false ->
                {false, ReqData, State1}
        end,
    {Forbidden, NewReqData, NewState}.

-spec produce_content(wrq:reqdata(), term()) ->
          {iodata(), wrq:reqdata(), term()}.
produce_content(ReqData, State) ->
    #state{clean_path=Path}=NewState = clean_path(ReqData, State),
    case file:read_file(Path) of
        {ok, IoData} ->
            {IoData, ReqData, NewState};
        {error, _Reason} ->
            {{halt, 500}, ReqData, NewState}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

%% Remove ".." from path, and memoize for future use.
-spec clean_path(wrq:reqdata(), #state{}) -> #state{}.
clean_path(ReqData, State=#state{dir=Dir, clean_path=undefined}) ->
    RawPath = wrq:disp_path(ReqData),
    Clean = remove_dots(RawPath),
    State#state{clean_path=filename:join([Dir|Clean])};
clean_path(_ReqData, State) ->
    %% return memoized value
    State.


%% Attempt to handle "./" and "../" in paths, returning a list of path
%% parts that can be filename:join'd together. Also strips leading,
%% trailing, and repeated "/".
-spec remove_dots(string()) -> [string()].
remove_dots(Path) ->
    remove_dots(string:tokens(Path, "/"), []).

%% Helper for remove_dots/1.
-spec remove_dots([string()], [string()]) -> [string()].
remove_dots([], Acc) ->
    %% that's the end of the path
    lists:reverse(Acc);
remove_dots([".."|Rest], Acc) ->
    case Acc of
        [_|Parent] ->
            %% go up one level
            remove_dots(Rest, Parent);
        [] ->
            %% already at the top - exactly what we're trying to protect
            remove_dots(Rest, Acc)
    end;
remove_dots(["."|Rest], Acc) ->
    %% just stay here
    remove_dots(Rest, Acc);
remove_dots([Head|Rest], Acc) ->
    %% string:tokens doesn't do silly things like leave empty lists if
    %% someone puts several slashes in a row, so anything else in the
    %% input stays
    remove_dots(Rest, [Head|Acc]).
