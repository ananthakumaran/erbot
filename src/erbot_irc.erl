-module(erbot_irc).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).
-export([start/0, stop/1, start_link/0]).

-compile(export_all).

-record(state, {socket, nick, publisher, users}).

start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:call(Pid, quit).

init([]) ->
    register(erbot, self()),
    self() ! start,
    {ok, #state{users=orddict:new()}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(quit, _From, S = #state{socket=Socket}) ->
    ok = gen_tcp:close(Socket),
    io:format("Going down"),
    {stop, normal, ok, S}.

handle_cast({send, Reply}, S = #state{socket=Socket}) ->
    gen_tcp:send(Socket, Reply),
    io:format(">>>> ~p~n", [Reply]),
    {noreply, S};
handle_cast(try_new_nick, S = #state{nick=Nick}) ->
    NewNick = Nick ++ "_",
    registerNick(NewNick),
    {noreply, S#state{nick=NewNick}};
handle_cast({add_members, Channel, Members}, S = #state{users=Users, publisher=Publisher}) ->
    NewUsers = case orddict:find(Channel, Users) of
		   {ok, OldMembers} -> orddict:store(Channel,
						     add_elements(OldMembers, Members),
						     Users);
		   error -> orddict:store(Channel, ordsets:from_list(Members), Users)
	       end,
    gen_event:notify(Publisher, {users, NewUsers}),
    {noreply, S#state{users=NewUsers}};
handle_cast(welcome, S) ->
    {ok, Channels} = application:get_env(channels),
    [join(self(), Channel) || Channel <- Channels],
    {noreply, S}.


handle_info(start, State) ->
    {ok, BotName} = application:get_env(bot_name),
    {ok, Host} = application:get_env(host),
    {ok, Port} = application:get_env(port),
    {ok, Socket} = gen_tcp:connect(Host, Port, [{packet, line}]),
    registerNick(BotName),

    {ok, Publisher} = gen_event:start_link(),
    {ok, Plugins} = application:get_env(plugins),
    [gen_event:add_handler(Publisher, M, [self(), Args]) || {M, Args} <- Plugins],

    {noreply, State#state{socket=Socket, nick=BotName, publisher=Publisher}};
handle_info({tcp, _Socket, Message}, State) ->
    io:format("<<<< ~p~n", [Message]),
    message(erbot_protocol:parse(strip_crlf(Message)), State),
    {noreply, State};
handle_info(Unknown, State) ->
    io:format("got unknown message ~p~n", [Unknown]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

strip_crlf(Str) ->
    string:substr(Str, 1, length(Str) - 2).

%% irc

reply(Message) ->
    reply(self(), Message).
reply(Pid, Message) ->
    Reply = Message ++ "\r\n",
    gen_server:cast(Pid, {send, Reply}).

registerNick(Name) ->
    reply("NICK " ++ Name),
    reply("USER " ++ Name ++ " 0 * :" ++ Name).

join(Pid, Channel) ->
    reply(Pid, "JOIN " ++ Channel).

members(Pid, Channel) ->
    reply(Pid, "NAMES " ++ Channel).

send_message(Pid, Target, Message) ->
    [reply(Pid, string:join(["PRIVMSG", Target, ":" ++ M], " ")) || M <- string:tokens(Message, "\n")].

to_us(Target, #state{nick=Nick}) ->
    Target =:= Nick.

message({"PING", ServerName}, _S) ->
    reply("PONG " ++ ServerName);
message({From, "PRIVMSG", Rest}, S = #state{publisher=Publisher}) ->
    {Nick, _Name, _Host} = erbot_protocol:user(From),
    {Target, ":" ++ Message} = erbot_protocol:split_space(Rest),
    case to_us(Target, S) of
	true ->
	    gen_event:notify(Publisher, {private_msg, Nick, Message});
	false ->
	    gen_event:notify(Publisher, {channel_msg, {Nick, Target}, Message})
    end;
message({From, "JOIN", Rest}, #state{publisher=Publisher}) ->
    {Nick, _Name, _Host} = erbot_protocol:user(From),
    ":" ++ Channel = Rest,
    gen_server:cast(self(), {add_members, Channel, [Nick]}),
    gen_event:notify(Publisher, {join, Nick, Channel}),
    ok;
message({_Prefix, "001", _Welcome}, _S) ->
    gen_server:cast(self(), welcome);
message({_Prefix, "433", _NicknameInUse}, _S) ->
    gen_server:cast(self(), try_new_nick);
message({_Prefix, "353", MembersList}, _S) ->
    {Channel, Members} = erbot_protocol:members(MembersList),
    gen_server:cast(self(), {add_members, Channel, Members}),
    ok;
message({_Prefix, _Command, _Params}, _S) ->
    ok.


%% Utils
add_elements(Set, [E | R]) ->
    add_elements(ordsets:add_element(E, Set), R);
add_elements(Set, []) ->
    Set.
