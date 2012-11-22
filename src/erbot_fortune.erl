-module(erbot_fortune).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, code_change/3, terminate/2,
	handle_info/2]).

init([Client, []]) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    {ok, Client}.
handle_event({Type, From, "!fortune" ++ _Rest}, Client)
  when Type == private_msg; Type == channel_msg ->
    erbot_irc:send_message(Client, From, fortune()),
    {ok, Client};
handle_event({_, From, _}, Client) ->
    case random:uniform(10) of
	5 -> erbot_irc:send_message(Client, From, fortune());
	_ -> ok
    end,
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

fortune() ->
    os:cmd("fortune").
