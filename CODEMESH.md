Tutorial: Don't let it crash! - Writing an air traffic simulator with Erlang OTP
======================================================================

We are all familiar to what happens on an airport (apart from the luggage handling), but are we able to simulate it? We'll take a closer look at the flying and touching ground part.
The focus of this tutorial will be landing airplanes, with them communicating with a control tower to negotiate when and where can they land. During the process, we'll get familiar with the following:

* The Erlang programming language, and a simple App's structure
* OTP basics, especially the behaviours
* Taking a closer look gen_server and gen_fsm
* Implement how a plane behaves
* Build a control tower that can manage a swarm of planes, guiding them to land
* Making processes talking to each other

## Objectives

Understand some of the OTP principles as well as introduction to functional thinking when solving problems.

## Target Audience

Beginners and intermediates who like to explore Erlang and/or Elixir more.

## Installation Instructions

* Erlang 18 - http://www.erlang.org/downloads/18.3
* Rebar - https://github.com/rebar/rebar
* Your favourite editor (Vim, right?)

About Máté
==========

Máté apart from enjoying building teams and products with all kinds of technology, occasionally doubles as a certified toddler tamer.

Github: @marjaimate

Twitter: @marjaimate

