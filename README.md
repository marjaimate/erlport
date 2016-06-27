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

After that there's a shell script that runs the erlang node for you, executing primarily our test function that spawns

* Run code with `./start.sh`


The Task
========

We will try to simulate a scenario where we have an airport and a bunch of planes trying to land to said airport. The airport has a control tower that accepts communication from planes. The planes are by default airborne and approaching the airport.

First they will ask for permission to land. If they get a positive response, they should be allowed to commense the landing sequence, then go to the hangar as they finished up their journey. If they get a negative response back from the control tower, they stay in air and try again later.

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

