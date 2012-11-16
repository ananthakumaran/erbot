-module(erbot_echo).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, code_change/3, terminate/2,
	handle_info/2]).

init([Client, []]) ->
    {ok, Client}.
handle_event({Type, From, "!echo " ++ Message}, Client)
  when Type == private_msg; Type == channel_msg ->
    erbot_irc:send_message(Client, From, "echo: " ++ Message),
    {ok, Client};
handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
