-module(erlport).

-export([start/0, start_link/0, stop/0, help/0]).

start() ->
    io:format("~n~n==== ERLPORT STARTED ====~n~n", []),
    help(),
    io:format("~n~n========~n~n", []).
    ok.

start_link() ->
    ok.

stop() ->
    ok.

help() ->
    io:format("==== HELP ====~n~n", []),
    io:format("Run your tests with the following function calls:~n", []),
    io:format("  control_tower:test_tower_setup(). %% See if gen_server started correctly, and we can open a landing strip ~n", []),
    io:format("  control_tower:test_single_plane(). %% See if we can start the control tower and create a plane instance as well ~n", []),
    io:format("  control_tower:test_single_plane_landing(). %% See if we can start the control tower and create a plane instance, and land it ~n", []),
    io:format("  control_tower:test_planes(). %% See if we can start the control tower and create 10 planes, and land them all ~n", []).
