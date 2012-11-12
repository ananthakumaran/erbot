-module(erbot_irc).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-export([start/0, stop/1, start_link/0]).

-compile(export_all).


start() ->
    gen_server:start(?MODULE, [], []).
start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:call(Pid, quit).

init([]) ->
    {ok, Socket} = gen_tcp:connect("irc.foonetic.net", 6667, [{packet, line}]),
    register("erbot"),
    {ok, Socket}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(quit, _From, Socket) ->
    ok = gen_tcp:close(Socket),
    io:format("Going down"),
    {stop, normal, ok, Socket}.

handle_cast({send, Reply}, Socket) ->
    gen_tcp:send(Socket, Reply),
    io:format(">>>> ~s~n", [Reply]),
    {noreply, Socket}.

handle_info({tcp, _Socket, Message}, State) ->
    io:format("<<<< ~p~n", [Message]),
    erbot_irc:message(self(), strip_crlf(Message)),
    {noreply, State};
handle_info(Unknown, State) ->
    io:format("got unknown message ~p~n", [Unknown]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

strip_crlf(Str) ->
    string:substr(Str, 1, length(Str) - 2).

%% irc stuffs
reply(Message) ->
    reply(self(), Message).
reply(Pid, Message) ->
    Reply = Message ++ "\r\n",
    gen_server:cast(Pid, {send, Reply}).

register(Name) ->
    reply("NICK " ++ Name),
    reply("USER " ++ Name ++ " 0 * :" ++ Name).

join(Pid, Channel) ->
    reply(Pid, "JOIN " ++ Channel).

message(Pid, "PING " ++ ServerName) ->
    reply(Pid, "PONG " ++ ServerName);
message(_Pid, _Data) ->
    ok.
