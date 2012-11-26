-module(erbot_fortune).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, code_change/3, terminate/2,
	handle_info/2]).

-record(state, {client, frequency}).

init([Client, [{frequency, Frequency}]]) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    {ok, #state{client=Client, frequency=Frequency}}.
handle_event({Type, From, "!fortune" ++ _Rest}, S=#state{client=Client})
  when Type == private_msg; Type == channel_msg ->
    erbot_irc:send_message(Client, From, fortune()),
    {ok, S};
handle_event({_, From, _}, S=#state{client=Client, frequency=Frequency}) ->
    case random:uniform(Frequency) of
	1 -> erbot_irc:send_message(Client, From, fortune());
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
