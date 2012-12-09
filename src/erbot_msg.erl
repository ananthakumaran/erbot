-module(erbot_msg).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, code_change/3, terminate/2,
         handle_info/2]).

-record(state, {client, notify}).

init([Client, []]) ->
    {ok, #state{client=Client, notify=orddict:new()}}.

handle_event({channel_msg, {Nick, _Channel}, "!msg " ++ Message}, S) ->
    [To|Msg] = string:tokens(Message, " "),
    {ok, S#state{notify=save_offline_msg(To, Nick, string:join(Msg, " "), S)}};
handle_event({join, Nick, Channel}, S) ->
    {ok, S#state{notify=send_offline_msgs(Nick, Channel, S)}};
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

save_offline_msg(To, From, Message, #state{notify=Notify}) ->
    case orddict:find(To, Notify) of
        {ok, Waiters} ->
            orddict:store(To, ordsets:add_element({To, From, Message}, Waiters), Notify);
        error ->
            orddict:store(To, ordsets:from_list([{To, From, Message}]), Notify)
    end.
send_offline_msgs(To, Channel, #state{notify=Notify, client=Client}) ->
    case orddict:find(To, Notify) of
        {ok, Waiters} ->
            msg_waiters(ordsets:to_list(Waiters), Client, Channel),
            orddict:erase(To, Notify);
        error -> Notify
    end.

msg_waiters([{To, From, Message} | R], Client, Channel) ->
    erbot_irc:send_message(Client, Channel, To ++ ": " ++ From ++ " says: " ++ Message),
    msg_waiters(R, Client, Channel);
msg_waiters([], _, _) -> ok.


%% help(Reply) ->
%%     Reply("sends message to an offline nick"),
%%     ok.
