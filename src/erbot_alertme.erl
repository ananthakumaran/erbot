-module(erbot_alertme).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, code_change/3, terminate/2,
	 handle_info/2]).

-record(state, {client, notify}).

init([Client, []]) ->
    {ok, #state{client=Client, notify=orddict:new()}}.
handle_event({channel_msg, {Nick, Channel}, "!alertme " ++ Message}, S=#state{client=Client}) ->
    Reply = fun(M) -> erbot_irc:send_message(Client, Channel, M) end,
    alertme(Nick, Message, Reply, S);
handle_event({join, Nick, Channel}, S = #state{notify=Notify, client=Client}) ->
    NewNotify = case orddict:find(Nick, Notify) of
		    {ok, Waiters} ->
			alert_waiters(ordsets:to_list(Waiters), Client, Channel, Nick),
			orddict:erase(Nick, Notify);
		    error -> Notify
		end,
    {ok, S#state{notify=NewNotify}};
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

alertme(Nick, Message, Reply, S=#state{notify=Notify}) ->
    User = string:strip(Message),
    NewNotify = case User == "help" of
		    true ->
			help(Reply),
			Notify;
		    false ->
			Reply("Yes sir."),
			add_waiter(Notify, Nick, User)
		end,
    {ok, S#state{notify=NewNotify}}.

add_waiter(Notify, Waiter, For) ->
    case orddict:find(For, Notify) of
	{ok, Waiters} ->
	    orddict:store(For, ordsets:add_element(Waiter, Waiters), Notify);
	error ->
	    orddict:store(For, ordsets:from_list([Waiter]), Notify)
    end.

alert_waiters([W | R], Client, Channel, Nick) ->
    erbot_irc:send_message(Client, W, "hi, " ++ Nick ++ " joined " ++ Channel),
    alert_waiters(R, Client, Channel, Nick);
alert_waiters([], _, _, _) -> ok.


help(Reply) ->
    Reply("alerts you when user 'X' joins the channel\neg !alertme X"),
    ok.
