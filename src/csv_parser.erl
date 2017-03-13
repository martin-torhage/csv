-module(csv_parser).
-on_load(init_nif/0).

-export([init/1,
         close/1,
         feed/2,
         set_capture/2,
         parse_one_row/1,
         parse/1]).

init(_) ->
    erlang:nif_error(nif_not_loaded).

close(_) ->
    erlang:nif_error(nif_not_loaded).

feed(_, _) ->
    erlang:nif_error(nif_not_loaded).

set_capture(_, _) ->
    erlang:nif_error(nif_not_loaded).

parse_one_row(_) ->
    erlang:nif_error(nif_not_loaded).

parse(_) ->
    erlang:nif_error(nif_not_loaded).

%% Internal

init_nif() ->
    File = filename:join([code:priv_dir(csv),
                          "csv_parser_nif"]),
    ok = erlang:load_nif(File, 0).
