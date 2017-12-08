# -*-Make-*-
#
# Copyright © Campanja AB 2012. All Rights Reserved.
ERL ?= erl
REBAR=./rebar3
LOCAL_DEPS=deps

.PHONY: nif get-deps compile test clean

nif: get-deps compile

# Since rebar is using a single deps directory, inhereted by the top
# rebar.config, the relative path to the deps will vary. Building the
# C code requires a static relative path to libcsv, so we fetch libcsv
# here with a new rebar instance where we remove the inherited deps
# path. The deps dir is also specified in rebar-libcsv.config.
get-deps:
	@echo "Fetching libcsv into local deps dir."
	@${REBAR} get-deps

# Compile the NIF. As a side-effect, the Erlang app will also be
# compiled.
compile: get-deps
	@echo "Building csv (including the NIF) with rebar2."
	@${REBAR} compile

test: compile
	@${REBAR} eunit skip_deps=true

clean:
	@${REBAR} clean --all
