-module(erbot).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _StartArgs) ->
    erbot_sup:start_link().

stop(_State) ->
    ok.

