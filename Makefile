# -*-Make-*-
#
# Copyright © Campanja AB 2012. All Rights Reserve.
ERL ?= erl
REBAR=bin/rebar

.PHONY: deps xref

all: deps compile xref

compile:
	@${REBAR} -j compile

compile-c:
	#gcc -shared -I/usr/local/lib/erlang/erts-5.9.3.1/include c_src/csvtest.c priv/libcsv.o -o priv/csv_parser.so
	#gcc -o priv/erlcsv_parser_nif.so -shared -I/usr/local/lib/erlang/usr/include -l/usr/local/lib/erlang/usr/lib -fpic -shared c_src/csvparser_nif.c
	gcc -Wall -arch i386 -arch x86_64 -I/usr/local/lib/erlang/usr/include -fPIC -bundle -flat_namespace -undefined suppress -o priv/erlcsv_parser_nif.so c_src/csvparser_nif.c

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
