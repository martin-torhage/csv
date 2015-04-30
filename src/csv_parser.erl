-module(csv_parser).
-on_load(init_nif/0).

-export([init/0,
         close/1,
         parse/2]).

init() ->
    {error, nif_not_loaded}.

close(_) ->
    {error, nif_not_loaded}.

parse(_, _) ->
    {error, nif_not_loaded}.

% Internal

init_nif() ->
    File = filename:join([code:priv_dir(csv),
                          "csv_parser_nif"]),
    ok = erlang:load_nif(File, 0).
