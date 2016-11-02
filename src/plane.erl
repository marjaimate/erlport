-module(plane).
-behaviour(gen_fsm).

% states
-export([
         start/1,
         start_link/1,
         permission_to_land/1,
         land/1,
         prepare_for_landing/2,
         in_air/2,
         rest/1,
         get_state/1,
         generate_flight_number/0
        ]).
% callbacks
-export([ init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-include_lib("include/airport.hrl").

% Start / Create planes
start(ControlTowerPid) ->
    Plane = create_plane(ControlTowerPid),
    gen_fsm:start(?MODULE, [Plane], []).

start_link(ControlTowerPid) ->
    Plane = create_plane(ControlTowerPid),
    gen_fsm:start_link(?MODULE, [Plane], []).

permission_to_land(PlanePid) ->
    gen_fsm:send_event(PlanePid, permission_to_land).

land(PlanePid) ->
    gen_fsm:send_event(PlanePid, land).

rest(PlanePid) ->
    gen_fsm:send_all_state_event(PlanePid, shutdown).

get_state(PlanePid) ->
    gen_fsm:sync_send_all_state_event(PlanePid, get_state).


%%%%%% FSM %%%%%%
init([Plane]) ->
    %% Instructions %%
    %%
    %%  - Create our plane and set with the first state in_air
    %% ------------ %%
    %%
    %% Code to fill in %%
    {ok, state_to_fill_in, Plane}.
    %% ------------ %%

in_air(permission_to_land, Plane) ->
    %% Instructions %%
    %%
    %%  - We need to ask the control tower whether we can land or not while the plane is airborne
    %%  - if we get the go ahead -> then transition to prepare_for_landing
    %%  - if we did not get the permission -> stay in_air
    %% ------------ %%
    %%
    %% Code to fill in %%
    CT = Plane#plane.control_tower_pid,
    Result = control_tower:permission_to_land(CT, Plane),
    io:format("[PLANE] Plane ~s asks tower ~p for permission to land. Got response ~p ~n", [Plane#plane.flight_number, CT, Result]),
    {next_state, in_air, Plane};
    %% ------------ %%

% Redirect all unexpected calls to in_air events
in_air(Event, Data) ->
    unexpected(Event, in_air),
    {next_state, in_air, Data}.

prepare_for_landing(land, Plane) ->
    %% Instructions %%
    %%
    %%  - Call the control tower to land the plane on the assigned landing strip
    %%  - Transition to state landed when finished
    %% ------------ %%
    %%
    %% Code to fill in %%
    {next_state, landed, Plane}.
    %% ------------ %%

%% Instructions %%
%%
%%  - Once the plane landed, we just allow it to terminate with a simple log message
%% ------------ %%
%%
%% Code to fill in %%
%% terminate(normal, landed, S=#plane{}) ->
%% ....
%% ------------ %%

terminate(_Reason, _StateName, _StateData) ->
    ok.

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n",
              [self(), Msg, State]).

handle_info(Info, StateName, Data) ->
    io:format("~p received unknown event ~p while in state ~p~n", [self(), Info, StateName]),
    {next_state, StateName, Data}.
handle_event(shutdown, _StateName, State) ->
    {stop, normal, State};
handle_event(Event, StateName, State) ->
    io:format("Plane receives an unknown global event: ~p ~n", [Event]),
    {next_state, StateName, State}.

handle_sync_event(get_state, _From, StateName, State) ->
    { reply, StateName, StateName, State };
handle_sync_event(Event, _From, StateName, _State) ->
    io:format("Plane receives an unknown global sync event: ~p ~n", [Event]),
    {reply, "You are not understood", Event, StateName}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%%%%%%%%%%%%%%%%
% Private methods
generate_flight_number() ->
    Codes = ["IE", "FR", "AF", "BA", "WZ", "BG"],
    Code = lists:nth(random:uniform(6), Codes),
    Num = integer_to_list(random:uniform(1000)),
    Code ++ Num.

create_plane(ControlTowerPid) ->
    FlightNumber = generate_flight_number(),
    #plane{flight_number=FlightNumber, control_tower_pid=ControlTowerPid}.

