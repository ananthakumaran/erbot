-module(erbot_history).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).
-compile(export_all).

-record(state, {client, dbpath}).
-record(message, {timestamp, nick, channel, message}).

-define(Table, history).

init([Client, [{dbpath, Path}]]) ->
    self() ! start,
    {ok, #state{client=Client, dbpath=Path}}.

handle_event({private_msg, Nick, "!log" ++ Query}, S=#state{client=Client}) ->
    Reply = fun(Message) -> erbot_irc:send_message(Client, Nick, Message) end,
    safe_search(Query, Reply),
    {ok, S};
handle_event({channel_msg, {_Nick, Channel}, "!log" ++ Query}, S=#state{client=Client}) ->
    Reply = fun(Message) -> erbot_irc:send_message(Client, Channel, Message) end,
    safe_search(Query, Reply),
    {ok, S};
handle_event({channel_msg, {Nick, Channel}, Message}, S) ->
    log(Nick, Channel, Message),
    {ok, S};
handle_event(_, S) ->
    {ok, S}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(start, State) ->
    initialize_db(State#state.dbpath),
    {ok, State};
handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

initialize_db(Path) ->
    ?Table = lets:new(?Table,
                      [ordered_set, public, named_table, {keypos, #message.timestamp},
                       {db, [{path, Path}, create_if_missing]}]),
    ok.


string_join([], _Seperator) ->
    "";
string_join([H], _Seperator) ->
    H;
string_join([F|[S|R]], Seperator) ->
    string_join([F ++ Seperator ++ S | R], Seperator).


fields() ->
    [{timestamp, '$1'},
     {nick, '$2'},
     {channel, '$3'},
     {message, '$4'}].

field_to_var(Field) ->
    {Field, proplists:get_value(Field, fields())}.

predicate(P) ->
    list_to_atom(P).

build_condition(Condition) ->
    [Field | [Predicate | Value]] = string:tokens(Condition, " "),
    case field_to_var(list_to_atom(Field)) of
        {F, undefined} -> erlang:error("unknown field " ++ F);
        {timestamp, Var} -> {predicate(Predicate), Var, triple_to_timestamp(gate:approxidate(string_join(Value, " ")))};
        {_, Var} -> {predicate(Predicate), Var, string_join(Value, " ")}
    end.

build_query(Query) ->
    try
        Conditions = string:tokens(Query, ","),
        [{#message{timestamp = '$1', nick = '$2', channel = '$3', message = '$4'},
          lists:map(fun build_condition/1, Conditions),
          ['$_']}]
    catch
        _Exception:Reason -> {error, Reason}
    end.

to_str(Any) ->
    io_lib:format("~p", [Any]).


format_time(Timestamp) ->
    {{Y, M, D}, {H, Min, _S}} = calendar:now_to_datetime(timestamp_to_triple(Timestamp)),
    io_lib:format("~2..0b/~2..0b/~4..0b ~2..0b:~2..0b", [D, M, Y, H, Min]).

format_message(#message{timestamp=Timestamp, nick=Nick, channel=Channel, message=Message}) ->
    io_lib:format("~s: ~s in ~s ~s", [format_time(Timestamp), Nick, Channel, Message]).

render([]) -> "no results";
render(Results) when length(Results) =< 5 ->
    string_join(lists:map(fun format_message/1, Results), "\n");
render(Results) ->
    integer_to_list(length(Results)) ++ " Results\n" ++
        erbot_utils:gist(string_join(lists:map(fun format_message/1, Results), "\n")).

search(Query, Reply) ->
    case build_query(Query) of
        {error, Reason} -> help(Reply, to_str({error, Reason}));
        %% TODO validate matchspec. this will throw exception on bad spec
        %% don't call it directly. use safe_search
        MatchSpec ->
            case lets:select_reverse(?Table, MatchSpec, 500) of
                {error, Reason} -> Reply(to_str({error, Reason}));
                '$end_of_table' -> Reply(render([]));
                {Results, _Continuation} -> Reply(render(Results))
            end
    end.

safe_search(Query, Reply) ->
    try
        search(Query, Reply)
    catch
        _Exception:Reason  -> Reply(to_str({error, Reason}))
    end.


log(Nick, Channel, Message) ->
    lets:insert(?Table, [#message{timestamp = triple_to_timestamp(erlang:now()),
                                  nick = Nick, channel = Channel, message = Message}]),
    ok.

triple_to_timestamp({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.

timestamp_to_triple(T) ->
    {T div 1000000000000, (T div 1000000) rem 1000000, T rem 1000000}.

help(Reply) ->
    Reply("\neg !log channel == #activesphere\neg !log timestamp >= yesterday, channel == #activesphere\navailable fields timestamp, channel, nick, message. by default will list the last 500 messages in desc order").

help(Reply, Error) ->
    Reply(Error),
    help(Reply).


%% test
std(Text) ->
    io:format("~s~n", [Text]).

search(Query) ->
    search(Query, fun std/1).
