-module(control_tower).
-behaviour(gen_server).

% public methods
-export([
         start_link/0,
         open_landing_strip/1,
         close_landing_strip/2,
         permission_to_land/2,
         land_plane/3,
         close_airport/1
        ]).
% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                  terminate/2, code_change/3]).
% test
-export([test_planes/0, test_tower_setup/0, test_single_plane/0, test_single_plane_landing/0]).

-include_lib("include/airport.hrl").
-define(PLANE_MODULE, plane).


%% Test code
%%
%% Test if we can setup a tower instance, i.e. load up the server
test_tower_setup() ->
    {ok, CT} = control_tower:start_link(),
    LS1 = control_tower:open_landing_strip(CT),
    io:format("Control Tower: ~p | Landing Strip: ~p", [CT, LS1]),
    timer:sleep(1000),
    control_tower:close_airport(CT),
    ok.

%%
%% Test if we can start up a tower and spawn a plane with it.
%%
test_single_plane() ->
    {ok, CT} = control_tower:start_link(),
    LS1 = control_tower:open_landing_strip(CT),
    io:format("Control Tower: ~p | Landing Strip: ~p", [CT, LS1]),
    Plane = get_plane(CT),
    io:format("~n -- plane: ~p~n", [Plane]),
    timer:sleep(1000),
    control_tower:close_airport(CT),
    ok.

%%
%% Test if we can start up a tower and spawn a plane with it as well as attempting to land it
%%
test_single_plane_landing() ->
    {ok, CT} = control_tower:start_link(),
    LS1 = control_tower:open_landing_strip(CT),
    io:format("Control Tower: ~p | Landing Strip: ~p", [CT, LS1]),
    Plane = get_plane(CT),
    io:format("~n -- plane: ~p~n", [Plane]),
    ?PLANE_MODULE:permission_to_land(Plane),
    ?PLANE_MODULE:land(Plane),

    timer:sleep(1000),
    control_tower:close_airport(CT),
    ok.

%% This is where we spawn 10 planes, setup a control tower and then attempt to land them all
%% ----------
test_planes() ->
    % Setup control tower
    {ok, CT} = control_tower:start_link(),
    LS1 = control_tower:open_landing_strip(CT),
    io:format("Control Tower: ~p | Landing Strip: ~p", [CT, LS1]),

    % Create 10 planes
    Planes = [get_plane(CT) || _ <- lists:seq(1, 10)],
    io:format("~n -- planes: ~p~n", [Planes]),

    ok = land_planes(Planes),

    timer:sleep(1000),
    control_tower:close_airport(CT),
    ok.

get_plane(CT) ->
    {ok, PID} = ?PLANE_MODULE:start(CT),
    PID.

land_planes([]) -> ok;
land_planes([Plane1 | Tail]) ->
    % Get the planes to land
    ?PLANE_MODULE:permission_to_land(Plane1),
    case ?PLANE_MODULE:get_state(Plane1) of
        prepare_for_landing ->
            %% if we got permission to land -> land the plane
            ?PLANE_MODULE:land(Plane1),
            land_planes(Tail);
        in_air ->
            %% no permission to land yet -> try again later
            land_planes(Tail ++ [Plane1])
    end.
%% ----------


%%% Client API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Synchronous call
open_landing_strip(Pid) ->
    gen_server:call(Pid, open_landing_strip).

%% Asynchronous call
close_landing_strip(Pid, LandingStrip = #landing_strip{}) ->
    gen_server:cast(Pid, {close_landing_strip, LandingStrip}).

%% Synchronous call
close_airport(Pid) ->
    gen_server:call(Pid, terminate).

%% Synchronous call
permission_to_land(Pid, Plane = #plane{}) ->
    gen_server:call(Pid, {permission_to_land, Plane}).

%% Asynchronous call
land_plane(Pid, Plane = #plane{}, LandingStrip = #landing_strip{}) ->
    gen_server:call(Pid, {land_plane, Plane, LandingStrip}).

%%%%% Gen server callbacks %%%%%

init([]) -> {ok, closed}. %% no treatment of info here!

%% Unexpected message
handle_info(Msg, LandingStrip) ->
    io:format("[TOWER] Unexpected message: ~p~n",[Msg]),
    {noreply, LandingStrip}.

%% Close landing strip
handle_cast({close_landing_strip, _LS}, LandingStrip=#landing_strip{free=true}) ->
    io:format("[TOWER] Closing landing strip ~p~n", [LandingStrip]),
    {noreply, closed};

handle_cast({close_landing_strip, _LS}, LandingStrip=#landing_strip{free=false}) ->
    io:format("[TOWER] Landing strip ~p occupied, reschedule close~n", [LandingStrip]),
    timer:sleep(100),
    gen_server:cast(self(), {close_landing_strip, LandingStrip}),
    {noreply, LandingStrip};

%% Landing a plane
%%
%% TODO
handle_cast({make_landing, Plane, _LS, From}, LandingStrip) ->
    %% Instructions %%
    %%
    %%  When we're making a landing, we need to do the following:
    %%  - Sleep for 3000ms as landing takes some time
    %%  - Add a log message about the fact that we landed
    %%  - Free up the landing strip
    %%  - Message the plane that it can rest now
    %%  - Return as an asynchronus call, with the appropriate state
    %% ------------ %%
    %%
    %% Code to fill in %%
    %%
    io:format("[TOWER] Plane ~p landed, freeing up runway ~p ~n", [Plane#plane.flight_number, LandingStrip#landing_strip.id]),
    {noreply, LandingStrip}.
    %% ------------ %%


%% Approach the runway
handle_call({land_plane, Plane, _LS}, From, LandingStrip) ->
    %% Instructions %%
    %%
    %%  When we're starting to land a plane, we need to do:
    %%  - The tower needs to log that a plane is approaching the runway / landing strip
    %%  - We call make_landing asynchornously within this state on the control tower
    %%  - Reply with an ok message
    %%
    %% ------------ %%
    % Mark the landing strip as occupied
    {reply, ok, LandingStrip};
    %% ------------ %%

%% Open a new landing strip, available for planes to land on
handle_call(open_landing_strip, _From, closed) ->
    NewLS = create_landing_strip(),
    io:format("[TOWER] Opening new landing strip ~p~n", [NewLS]),
    {reply, NewLS, NewLS};

%% Check if the plane can land, look for free landing strips
%% Instructions %%
%%
%%  Asking for permission to land
%%  - We need to handle to cases here:
%%   - When the landing strip is free -> permission granted,
%%      but make sure we mark the landing strip as reserved for the plane that asked for it
%%   - If the landing strip is occupied, we need to return with
%%      a cannot_land message so the plane can retry later
%%
%% ------------ %%
handle_call({permission_to_land, Plane = #plane{}}, _From, LandingStrip) ->
    {reply, LandingStrip, LandingStrip};

%% ------------ %%

%% Close the airport
handle_call(terminate, _From, LandingStrip) ->
    {stop, normal, ok, LandingStrip}.

%% Free up all the landing strips
terminate(normal, LandingStrip) ->
    io:format("[TOWER] Landing Strip ~p was freed up.~n",[LandingStrip]),
    ok.

%% Code upgrade
code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

%% Private functions
create_landing_strip() ->
    #landing_strip{id = random:uniform(1000000)}.
