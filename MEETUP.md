Setup
=====

Before coming to the workshop, it would be advised to have Erlang installed. You can do it either [through installers](http://www.erlang.org/downloads) directly OR using Kerl:

* Install [kerl](https://github.com/yrashk/kerl)
* Setup your `~/.kerlrc` with
```bash
export KERL_BASE_DIR="$HOME/.kerl"
export KERL_DEFAULT_INSTALL_DIR="$KERL_BASE_DIR/installs"
```
* Install Erlang [version 18.0](http://www.erlang.org/download/otp_src_18.0.tar.gz) and activate it
```bash
$ kerl build 18.0 18.0
$ kerl install 18.0
$ . $KERL_DEFAULT_INSTALL_DIR/18.0/activate
```

Workshop
=======

Workshop will be about getting an introduction to Erlang. This is a langauge and system that has been around the block for some time now.

Erlang was built on the premise to start applications once and run pretty much forever (read: when the hardware gives up). The language and the VM that runs your apps are super durable and can withstand serious traffic spikes and surges in computing with ease.

During the workshop we're going to take a quick look at how Erlang works and try it ourselves:

* Syntax 101 - don't let the dots scare you
* Pattern matching
* Message passing
* OTP
* Compile & run your app
* Complete a small coding exercise using all of the above

Reading material
================

* Learn you some Erlang - http://learnyousomeerlang.com
* Getting started with Erlang - http://erlang.org/download/getting_started-5.4.pdf
* Erlang basics - http://alancastro.org/2010/04/26/erlang-basics.html
