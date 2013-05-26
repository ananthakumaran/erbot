-module(erbot_utils).

-export([gist/1]).

gist(Text) ->
    Url = "https://api.github.com/gists",
    Body = {[{public, false},
	     {files, {[{'output.txt', {[{content, list_to_binary(Text)}]}}]}}]},
    Request = {Url, [{"User-Agent", "Erbot"}], "text/json", ejson:encode(Body)},
    {ok, {_, _Headers, Result}} = httpc:request(post, Request, [], []),
    {Results} = ejson:decode(Result),
    {_, {[{<<"output.txt">>, {FileAttributes}}]}} = lists:keyfind(<<"files">>, 1, Results),
    {_, GistUrl} = lists:keyfind(<<"raw_url">>, 1, FileAttributes),
    binary_to_list(GistUrl).
