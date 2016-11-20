-module(csv_parser).
-on_load(init_nif/0).

-export([init/0,
         close/1,
         feed/2,
         parse/1]).

init() ->
    erlang:nif_error(nif_not_loaded).

close(_) ->
    erlang:nif_error(nif_not_loaded).

feed(_, _) ->
    erlang:nif_error(nif_not_loaded).

parse(_) ->
    erlang:nif_error(nif_not_loaded).

%% Internal

init_nif() ->
    File = filename:join([code:priv_dir(csv),
                          "csv_parser_nif"]),
    ok = erlang:load_nif(File, 0).
