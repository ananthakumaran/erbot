-module(erbot_protocol).

-export([parse/1, split_space/1, user/1, members/1]).

parse(M = ":" ++ _Rest) ->
    parse_prefix(M);
parse(Message) ->
    parse_command(Message).

split_space(M) ->
    case re:run(M, "(.+?) (.+)", [{capture, [1, 2], list}]) of
	{match, [First, Rest]} ->
	    {First, Rest};
	nomatch -> error
    end.

user(MsgTarget) ->
    case re:run(MsgTarget, ":(.+)!(.+)@(.+)", [{capture, [1, 2, 3], list}]) of
	{match, [Nick, Name, Host]} ->
	    {Nick, Name, Host};
	nomatch -> error
    end.

parse_prefix(M) ->
    {Prefix, Rest} = split_space(M),
    case parse_command(Rest) of
	{Command, Params} ->
	    {Prefix, Command, Params};
	error -> error
    end.

parse_command(M) ->
    split_space(M).

members(M) ->
    case re:run(M, ".+ [=*@] (.+) :[^ ]* (.+)", [{capture, [1, 2], list}]) of
	{match, [Channel, Members]} ->
	    {Channel, string:tokens(Members, " ")};
	nomatch -> error
    end.
