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
    SoFileName = filename:join([priv_dir(),
                                "csv_parser_nif"]),
    ok = erlang:load_nif(SoFileName, 0).

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            %% Development mode fallback: assume priv/ is sibling to ebin/
            EbinDir = filename:dirname(code:which(?MODULE)),
            filename:join(EbinDir, "../priv");
        Path when is_list(Path) ->
            Path
    end.
