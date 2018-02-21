%% Copyright 2018 VMware, all rights reserved.
%%
%% Static file serving resource. Serves files from /priv/www, unless a
%% "dir" or "file" property in the init list points elsewhere.
%%
%% In the case of "file", exactly that file will be served, regardless
%% of the dispatch path. The dispatch rule:
%%
%% {["anything",'*'], helen_static_resource, [{file, "/foo/bar.html"}]}
%%
%% will serve the content of "/foo/bar.html" to requests for
%% "/anything", "/anything/baz", "/anything/quux/yadda", etc.
%%
%% In the case of "dir", files within (including subdirectories) are
%% served, based on the dispatch path. The dispatch rule:
%%
%% {["foo", '*'], helen_static_resource, [{dir, "/bar"}]}
%%
%% will serve requests like the following:
%%
%%  Requested Path        | File delivered
%%  -------------------------------
%%  /foo/                 | /bar/index.html
%%  /foo/image.png        | /bar/image.png
%%  /foo/baz/quux/test.js | /bar/baz/quux/test.js
%%
%% The default, no-config case is equivalent to [{dir, "priv/www"}].
%%
%% TODO: add caching headers

-module(helen_static_resource).
-export([
         init/1,
         resource_exists/2,
         previously_existed/2,
         moved_permanently/2,
         moved_temporarily/2,
         content_types_provided/2,
         forbidden/2,
         produce_content/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {
          dir :: string(),
          clean_path :: string(),
          type :: nothing | file | dir_with_index | dir_without_index
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init([{dir,string()}]) -> {ok|{trace, string()}, #state{}}.
init(Props) ->
    case lists:keyfind(file, 1, Props) of
        {file, F} ->
            %% purposely leaving `dir` undefined here, to catch
            %% potential places where this forced path could be
            %% ignored
            {ok, #state{clean_path=F, type=file}};
        false ->
            Dir = case lists:keyfind(dir, 1, Props) of
                      {dir, D} -> D;
                      false -> filename:join([code:priv_dir(helen), "www"])
                  end,
            {ok, #state{dir=Dir}}
    end.

-spec resource_exists(wrq:reqdata(), #state{}) ->
          {boolean(), wrq:reqdata(), #state{}}.
resource_exists(ReqData, State) ->
    NewState = inspect_path(ReqData, State),
    Exists = case NewState#state.type of
                 nothing -> false;
                 file -> true;
                 _ ->
                     case lists:reverse(wrq:path(ReqData)) of
                         [$/|_] -> true;
                         _ ->
                             %% web browsers get very confused with
                             %% sub paths if the URL does not end with
                             %% a /, so help them by redirecting (see
                             %% previously_existed and
                             %% moved_temporarily)
                             false
                     end
             end,
    {Exists, ReqData, NewState}.

-spec previously_existed(wrq:reqdata(), #state{}) ->
          {boolean()|{halt,500}, wrq:reqdata(), #state{}}.
previously_existed(ReqData, State) ->
    NewState = inspect_path(ReqData, State),
    Existed = case NewState#state.type of
                  dir_with_index ->
                      %% resource_exists returned false because the
                      %% URL doesn't end with a /, so we'll redirect
                      %% from moved_temporarily
                      true;
                  _ ->
                      % neither nothing nor dir_without_index doesn't
                      % gets a redirect (and 'file' shouldn't get here)
                      false
              end,
    {Existed, ReqData, NewState}.

-spec moved_permanently(wrq:reqdata(), #state{}) ->
          {false, wrq:reqdata(), #state{}}.
moved_permanently(ReqData, State) ->
    %% permanent move limits our future options without having to ask
    %% users to clear their caches
    {false, ReqData, State}.

-spec moved_temporarily(wrq:reqdata(), #state{}) ->
          {{true, URI::string()}, wrq:reqdata(), #state{}}.
moved_temporarily(ReqData, State) ->
    NewState = inspect_path(ReqData, State),
    %% insisting we've gotten here only one way
    dir_with_index = NewState#state.type,

    %% help out browser apps that redirect to /blah instead of /blah/,
    %% and then have /blah/index.html reference /foo.jpg instead of
    %% /blah/foo.jpg.
    Path = wrq:path(ReqData)++"/",
    {{true, Path}, ReqData, NewState}.

-spec content_types_provided(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    #state{clean_path=Path}=NewState = inspect_path(ReqData, State),
    Type = case NewState#state.type of
               file ->
                   webmachine_util:guess_mime(Path);
               _ ->
                   %% We're serving either .../index.html or an error
                   "text/html"
           end,
    {[{Type, produce_content}], ReqData, NewState}.

-define(NO_LISTING,
        <<"<html><head><title>403 Forbidden</title></head>",
          "<body><h1>403 Forbidden</h1>",
          "<p>Directory listing is forbidden</p></body></html>">>).

-spec forbidden(wrq:reqdata(), #state{}) ->
          {boolean(), wrq:reqdata(), #state{}}.
forbidden(ReqData, State) ->
    NewState = inspect_path(ReqData, State),
    {Forbidden, NewReqData} =
        case NewState#state.type of
            dir_without_index ->
                %% listing of directories is not supported
                {true, wrq:set_resp_body(?NO_LISTING, ReqData)};
            _ ->
                {false, ReqData}
        end,
    {Forbidden, NewReqData, NewState}.

-spec produce_content(wrq:reqdata(), term()) ->
          {iodata(), wrq:reqdata(), term()}.
produce_content(ReqData, State) ->
    NewState = inspect_path(ReqData, State),
    RealPath = case NewState#state.type of
                   file ->
                       NewState#state.clean_path;
                   dir_with_index ->
                       filename:join([NewState#state.clean_path, "index.html"]);
                   _ ->
                       %% we never get here - make sure this throws an
                       %% error below if we do by accident
                       undefined
               end,
    case file:read_file(RealPath) of
        {ok, IoData} ->
            {IoData, ReqData, NewState};
        {error, _Reason} ->
            {{halt, 500}, ReqData, NewState}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

%% Map URL to file. Remove problematic components (., .., //),
%% determine type of result, and memoize everything for future use.
-spec inspect_path(wrq:reqdata(), #state{}) -> #state{}.
inspect_path(ReqData, State=#state{dir=Dir, clean_path=undefined}) ->
    RawPath = wrq:disp_path(ReqData),
    Clean = filename:join([Dir|remove_dots(RawPath)]),
    Type = case filelib:is_regular(Clean) of
               true ->
                   file;
               false ->
                   case filelib:is_dir(Clean) of
                       true ->
                           case filelib:is_regular(
                                  filename:join([Clean,"index.html"])) of
                               true ->
                                   dir_with_index;
                               false ->
                                   dir_without_index
                           end;
                       false ->
                           nothing
                   end
           end,
    State#state{clean_path=Clean, type=Type};
inspect_path(_ReqData, State) ->
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
