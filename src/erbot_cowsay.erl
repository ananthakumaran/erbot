-module(erbot_cowsay).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2,
	 handle_info/2, terminate/2, code_change/3]).

init([Client, []]) ->
    {ok, Client}.

handle_event({private_msg, Nick, "!cowsay " ++ Message}, Client) ->
    cowsay(Nick, Message, Client),
    {ok, Client};
handle_event({channel_msg, {_Nick, Channel}, "!cowsay " ++ Message}, Client) ->
    cowsay(Channel, Message, Client),
    {ok, Client};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cowsay(To, Message, Client) ->
    Cowsay = os:cmd("cowsay " ++ Message),
    erbot_irc:send_message(Client, To, Cowsay).
