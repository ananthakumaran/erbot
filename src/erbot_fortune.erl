-module(erbot_fortune).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, code_change/3, terminate/2,
	handle_info/2]).

-record(state, {client, frequency}).

init([Client, [{frequency, Frequency}]]) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    {ok, #state{client=Client, frequency=Frequency}}.

handle_event({private_msg, Nick, "!fortune" ++ _Rest}, S=#state{client=Client}) ->
    send_fortune(Client, Nick),
    {ok, S};
handle_event({channel_msg, {_Nick, Channel}, "!fortune" ++ _Rest}, S=#state{client=Client}) ->
    send_fortune(Client, Channel),
    {ok, S};

handle_event({channel_msg, {_Nick, Channel}, _}, S=#state{client=Client, frequency=Frequency}) ->
    case random:uniform(Frequency) of
	1 -> send_fortune(Client, Channel);
	_ -> ok
    end,
    {ok, S};
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

fortune() ->
    os:cmd("fortune").

send_fortune(Client, To) ->
    erbot_irc:send_message(Client, To, fortune()).
