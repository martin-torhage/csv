# -*-Make-*-
#
# Copyright © Campanja AB 2012. All Rights Reserved.
ERL ?= erl
REBAR=bin/rebar

.PHONY: deps xref

all: deps compile xref

compile:
	@${REBAR} -j compile

test: all
	@${REBAR} -j eunit skip_deps=true

xref: compile
	@${REBAR} -j xref skip_deps=true

deps:
	@${REBAR} -j get-deps

clean:
	@${REBAR} -j clean
	rm -fr logs

dist-clean: clean
	@${REBAR} -j delete-deps

# Since rebar is using a single deps directory, inhereted by the top
# rebar.config, the relative path to the deps will vary. Building the
# C code requires a static relative path to libcsv, so we fetch libcsv
# here with a new rebar instance where we remove the inherited deps
# path. The deps dir is also specified in rebar-libcsv.config.
get-libcsv:
	@echo "Fetching libcsv into local deps dir."
	@REBAR_DEPS_DIR=deps ${REBAR} -C rebar-libcsv.config get-deps
