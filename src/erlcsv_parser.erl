-module(erlcsv_parser).
-on_load(init_nif/0).

-export([world/0,
         parse/1]).

world() ->
    nif_not_loaded.

parse(_) ->
    nif_not_loaded.

init_nif() ->
    ok = erlang:load_nif("./priv/erlcsv_parser_nif", 0).
