-module(erbot).
-behaviour(application).
-export([start/2, start/0, stop/1]).

dependencies() ->
    [sasl, crypto, asn1, public_key, ssl, inets].

start() ->
    [application:start(M) || M <- dependencies() ++ [erbot]].

start(normal, _StartArgs) ->
    erbot_sup:start_link().

stop(_State) ->
    ok.
