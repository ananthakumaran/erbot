-module(erbot_capistrano).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2,
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {client, apps=[]}).

init([Client, Apps]) ->
    {ok, #state{client=Client, apps=Apps}}.

handle_event({Type, From, "!cap " ++ Rest}, S=#state{client=Client, apps=Apps})
  when Type == private_msg; Type == channel_msg ->
    Reply = fun(Message) -> erbot_irc:send_message(Client, From, Message) end,
    case re:run(Rest, "(.+?) (.+)", [{capture, [1, 2], list}]) of
	{match, [App, Cmd]} ->
	    cap(App, Cmd, Apps, Reply);
	nomatch ->
	    Reply("bad command. eg !cap app showcase deploy")
    end,
    {ok, S};
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

cap(App, Cmd, Apps, Reply) ->
    case lists:keyfind(list_to_atom(App), 1, Apps) of
	{_, Dir} ->
	    run_cap_command(Dir, Cmd, Reply);
	false ->
	    Reply("app " ++ App ++ " not found")
    end.

run_cap_command(Dir, Cmd, Reply) ->
    CapCommand = "bash -l -c 'git pull && bundle exec cap " ++ Cmd ++ "'",
    case eunit_lib:command(CapCommand, Dir) of
	{0, _Out} ->
	    Reply("command finished successfully");
	{ExitCode, Out} ->
	    Reply(lists:concat(["command exited with status ", ExitCode])),
	    Reply(Out)
    end.
