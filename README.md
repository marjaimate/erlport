Setup
=====

Before coming to the workshop, it would be advised to have Erlang installed. You can do it either [through installers](http://www.erlang.org/downloads) directly OR using Kerl:

* Install [kerl](https://github.com/yrashk/kerl)
* Setup your ~/.kerlrc with
```bash
$ export KERL_BASE_DIR="$HOME/.kerl"
$ export KERL_DEFAULT_INSTALL_DIR="$KERL_BASE_DIR/installs"
```
* Install Erlang [version 18.0](http://www.erlang.org/download/otp_src_18.0.tar.gz) and activate it
```bash
$ kerl build 18.0 18.0
$ kerl install 18.0
$ . $KERL_DEFAULT_INSTALL_DIR/18.0/activate
```

## How to check I'm all setup?

If you try to run the Erlang shell with simply `erl` should be enough.

Running the code
=================

To run our code and see the results, we're going to use [rebar](https://github.com/rebar/rebar). Rebar is an Erlang build tool that makes it easy to compile and test Erlang applications, port drivers and releases.

* Compile code `./rebar compile`

After that there's a shell script that runs the erlang node for you:

* Run code with `./start.sh`

This will boot up the Erlang VM with a shell where you can call your code as well as execute some prepared tests for the following task:

The Task
========

We will try to simulate a scenario where we have an airport and a bunch of planes trying to land to said airport. The airport has a control tower that accepts communication from planes. The planes are by default airborne and approaching the airport.

First they will ask for permission to land. If they get a positive response, they should be allowed to commence the landing sequence, then go to the hangar as they finished up their journey. If they get a negative response back from the control tower, they stay in air and try again later.

In our case, the *control tower* is a [gen_server](http://erlang.org/doc/man/gen_server.html), listening and reacting to events. It also keeps a state data structure of itself to keep track of the landing strip. The server accepts the following messages / calls:

* **open_landing_strip** - Create a landing strip and register it in the server's state
* **close_landing_strip** - Close down the landing strip
* **close_airport** - Close down the airport
* **permission_to_land** - Response with a state / landing strip id whether the plane which made the request can land
* **land_plane** - Land the plane and free up the runway

*Planes* are instances of a [gen_fsm](http://erlang.org/doc/man/gen_fsm.html). This finite state machine has the following states:

* **in_air** - The plane is airborne, waiting to land. Possible actions / transitions: *permission_to_land*
* **prapre_for_landing** - Got permission to land and attempts landing to the given landing strip. Possible actions / transitions: *land*
* **landed** - Plane has touched ground and ready to finish its journey. Possible actions / transitions: *terminate*

## Tests

### Initialize the tower

After starting the node, you need to call `control_tower:test_tower_setup/0`:

```bash
====NODE STARTED=====

Eshell V6.4  (abort with ^G)
1> control_tower:test_tower_setup().
[TOWER] Opening new landing strip {landing_strip,443585,true}
Control Tower: <0.34.0> | Landing Strip: {landing_strip,443585,true}[TOWER] Landing Strip {landing_strip,443585,true} was freed up.
ok
2>
```

### Testing plane creation

First thing first is to make sure we can open and close the tower. Now, we need to take a look at the planes. First all we need is to make sure we can create a Plane process with the correct initial state. Once we have that we can call `control_tower:test_single_plane/0` the same way as above.

### Test single plane landing

So we have a basic tower, and a basic plane. Now all we need is the two of them working together in order to complete a landing sequence. To complete that, we need to do the following:

- [ ] On the *Plane* implement the missing part in the transition from *in_air* x *permission_to_land* as well as the corresponding part on the *Control Tower*.
  - [ ] Make the synchronous call from the Plane FSM to the Control tower.
  - [ ] Handle the call in the Control tower by checking the available landing strip(s)
  - [ ] Reserve the landing strip for the plane
  - [ ] Interpret the result on the Plane side and make a state transition accordingly
- [ ] On the *Plane* side, implement the transition *prepare_for_landing* x *land*
  - [ ] Call the Control tower to make the landing
  - [ ] Transition to state in the end.
- [ ] On the *Control Tower* implement the *land_plane* and *make_landing* calls
  - [ ] *land_plane* is a synchronous call, we should return with an acknoledgement and call the async *make_landing*
  - [ ] Add some sleep time, as landing take some effort to complete
  - [ ] Add a log message about the plane landed
  - [ ] Free up the landing strip
- [ ] On the *Plane* side, close our process with the *terminate* call where we send our planes to the hangar for some rest.

You can test out your progress by calling `control_tower:test_single_plane_landing/0` from the Erlang shell.


### Test multiple planes landing

Now, we have completed a single plane case, let's see if our control tower can handle multiple planes approaching the same time! Call `control_tower:test_planes/0` and see how your solution works!
