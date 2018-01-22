# -*-Make-*-
#
# Copyright © Campanja AB 2012. All Rights Reserved.
ERL ?= erl
REBAR=./rebar3
LOCAL_DEPS=deps

.PHONY: upgrade compile test clean

# Since rebar is using a single deps directory, inhereted by the top
# rebar.config, the relative path to the deps will vary. Building the
# C code requires a static relative path to libcsv, so we fetch libcsv
# here with a new rebar instance where we remove the inherited deps
# path. The deps dir is also specified in rebar-libcsv.config.
upgrade:
	@echo "Fetching libcsv into local deps dir."
	@${REBAR} upgrade

# Compile the NIF. As a side-effect, the Erlang app will also be
# compiled.
compile: upgrade
	@echo "Building csv (including the NIF) with rebar3."
	@${REBAR} compile

test: compile
	@${REBAR} eunit skip_deps=true

clean:
	@${REBAR} clean --all
