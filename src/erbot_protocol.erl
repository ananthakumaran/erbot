-module(erbot_protocol).

-export([parse/1]).

parse(M = ":" ++ _Rest) ->
    parse_prefix(M);
parse(Message) ->
    parse_command(Message).


parse_prefix(M) ->
    case re:run(M, "(.+?) (.+)", [{capture, [1, 2], list}]) of
	{match, [Prefix, Rest]} ->
	    case parse_command(Rest) of
		{Command, Params} ->
		    {Prefix, Command, Params};
		error -> error
	    end;
	nomatch -> error
    end.

parse_command(M) ->
    case re:run(M, "(.+?) (.+)", [{capture, [1, 2], list}]) of
	{match, [Command, Params]} ->
	    {Command, Params};
	nomatch -> error
    end.
