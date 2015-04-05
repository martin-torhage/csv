-module(erlcsv_parser).
-on_load(init_nif/0).

-export([world/0]).

world() ->
      "NIF library not loaded".

init_nif() ->
    ok = erlang:load_nif("./priv/erlcsv_parser_nif", 0).
