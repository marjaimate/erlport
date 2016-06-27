-module(erlport).

-export([start/0, start_link/0, stop/0]).

start() ->
    io:format("~n~n=========~n~n", []),
    control_tower:test2(),
    ok.

start_link() ->
    ok.

stop() ->
    ok.
