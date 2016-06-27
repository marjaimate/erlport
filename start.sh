#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -s erlport start -config ./priv/erlport
