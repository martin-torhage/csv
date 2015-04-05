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
