-module(erbot_sup).
-behaviour(supervisor).
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    MaxRestart = 5,
    MaxTime = 100,
    {ok, {{one_for_one, MaxRestart, MaxTime},
          [{irc,
	    {erbot_irc, start_link, []},
	    permanent,
	    60000,
	    worker,
	    [erbot_irc]}]}}.
