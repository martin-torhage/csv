# -*-Make-*-
#
# Copyright © Campanja AB 2012. All Rights Reserved.
ERL ?= erl
REBAR=bin/rebar
LOCAL_DEPS=deps

.PHONY: nif get-nif-deps nif-compile test

nif: get-nif-deps nif-compile

# Since rebar is using a single deps directory, inhereted by the top
# rebar.config, the relative path to the deps will vary. Building the
# C code requires a static relative path to libcsv, so we fetch libcsv
# here with a new rebar instance where we remove the inherited deps
# path. The deps dir is also specified in rebar-libcsv.config.
get-nif-deps:
	@echo "Fetching libcsv into local deps dir."
	@REBAR_DEPS_DIR="${LOCAL_DEPS}" ${REBAR} -C rebar-nif.config get-deps

# Compile the NIF. As a side-effect, the Erlang app will also be
# compiled.
nif-compile:
	@echo "Building csv (including the NIF) with rebar2."
	@REBAR_DEPS_DIR="${LOCAL_DEPS}" ${REBAR} -C rebar-nif.config compile

test: nif
	@${REBAR} -j eunit skip_deps=true
