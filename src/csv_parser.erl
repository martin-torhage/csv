-module(csv_parser).
-on_load(init_nif/0).

-export([init/0,
         close/1,
         parse/2]).

init() ->
    nif_not_loaded.

close(_) ->
    nif_not_loaded.

parse(_, _) ->
    nif_not_loaded.

% Internal

init_nif() ->
    ok = erlang:load_nif("./priv/csv_parser_nif", 0).
