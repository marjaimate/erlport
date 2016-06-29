-module(erlport).

-export([start/0, start_link/0, stop/0]).

start() ->
    io:format("~n~n====NODE STARTED=====~n~n", []),
    ok.

start_link() ->
    ok.

stop() ->
    ok.
