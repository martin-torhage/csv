# -*-Make-*-
#
# Copyright © Campanja AB 2012. All Rights Reserved.
ERL ?= erl
REBAR=./rebar3
LOCAL_DEPS=deps

.PHONY: upgrade compile test clean

all: compile

upgrade:
	@${REBAR} upgrade

compile: upgrade
	@echo "Building csv (including the NIF) with rebar3."
	@${REBAR} compile

test: compile
	@${REBAR} eunit skip_deps=true

clean:
	@${REBAR} clean --all
