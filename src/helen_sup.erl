-module(helen_sup).
-behaviour(supervisor).

%% External exports
-export([
  start_link/0
]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [helen_config:web_config()]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Athena = {helen_athena_sup,
              {helen_athena_sup, start_link, []},
              permanent, 5000, supervisor, [helen_athena_sup]},
    Processes = [Web, Athena],
    {ok, { {one_for_one, 10, 10}, Processes} }.
