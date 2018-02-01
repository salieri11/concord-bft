-module(helen_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    helen_sup:start_link().

stop(_State) ->
    ok.
