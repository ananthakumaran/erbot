-module(erbot_echo).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, code_change/3, terminate/2,
	handle_info/2]).

init([Client, []]) ->
    {ok, Client}.
handle_event({private_msg, Nick, "!echo " ++ Message}, Client) ->
    erbot_irc:send_message(Client, Nick, "echo: " ++ Message),
    {ok, Client};
handle_event({channel_msg, {_Nick, Channel}, "!echo " ++ Message}, Client) ->
    erbot_irc:send_message(Client, Channel, "echo: " ++ Message),
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
