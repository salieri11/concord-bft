%% Copyright 2018 VMware, all rights reserved.
%%
%% Supervisor to monitor Athena connections.

-module(helen_athena_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Children = [#{id => athena_conn, %% TODO: we will have several
                  start => {helen_athena_conn, start_link, [N]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [helen_athena_conn]} || N <- athena_nodes()],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

athena_nodes() ->
    case application:get_env(helen, athena_nodes) of
        {ok, Nodes} ->
            [ N || {_Ip, _Addr}=N <- Nodes ];
        undefined ->
            []
    end.
