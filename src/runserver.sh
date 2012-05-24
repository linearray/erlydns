#!/bin/sh

ERL=`which erl`
MNESIA_DIR=mnesiadata
ERLANG_NODE=erlydns


$ERL -setcookie franz -sname $ERLANG_NODE -env ERL_MAX_ETS_TABLES 500000 -mnesia dir $MNESIA_DIR \
-s server start 1053
