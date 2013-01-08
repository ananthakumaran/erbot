-module(erbot_capistrano).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2,
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {client, apps=[]}).

init([Client, Apps]) ->
    {ok, #state{client=Client, apps=Apps}}.

handle_event({channel_msg, {_Nick, Channel}, "!cap " ++ Rest}, S=#state{client=Client, apps=Apps}) ->
    Reply = fun(Message) -> erbot_irc:send_message(Client, Channel, Message) end,
    case re:run(Rest, "(.+?) (.+)", [{capture, [1, 2], list}]) of
	{match, [App, Cmd]} ->
	    spawn(fun() -> cap(App, Cmd, Apps, Reply) end);
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
    CapCommand = "bash -l -c 'git pull && bundle install --deployment && bundle exec cap " ++ Cmd ++ "'",
    Reply(CapCommand),
    case eunit_lib:command(CapCommand, Dir) of
	{0, Out} ->
	    Reply("command finished successfully"),
	    Reply(erbot_utils:gist(Out));
	{ExitCode, Out} ->
	    Reply(lists:concat(["command exited with status ", ExitCode])),
	    Reply(erbot_utils:gist(Out))
    end.
