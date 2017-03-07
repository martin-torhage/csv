-module(csv_tests).

-include_lib("eunit/include/eunit.hrl").

csv_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"Should decode a binary",
       ?_test(decode_binary())},
      {"Should decode to binary",
       ?_test(decode_to_binary())},
      {"Should decode tab separated",
       ?_test(decode_tabbed_binary())},
      {"Should decode only selected columns",
       ?_test(decode_selected_columns())},
      {"Should handle out of bounds capture",
       ?_test(decode_out_of_bounds_capture())},
      {"Should decode only commas chars",
       ?_test(decode_commas_only())},
      {"Should decode max amount of rows",
       ?_test(decode_max_rows())}]}.

setup() ->
    ok.

teardown(ok) ->
    ok.

%% Tests

decode_binary() ->
    Csv = <<"col1,col2,\"col with a \"\"\",col4\n"
            "only column in row 2">>,
    Expected = [["col1", "col2", "col with a \"", "col4"],
                ["only column in row 2"]],
    Actual = csv:decode_binary(Csv),
    ?assertEqual(Expected, Actual).

decode_to_binary() ->
    Csv = <<"col1,col2,col3\n"
            "val1,val2,val3">>,
    Expected = [[<<"col1">>, <<"col2">>, <<"col3">>],
                [<<"val1">>, <<"val2">>, <<"val3">>]],
    Options = [{return, binary}],
    Actual = csv:decode_binary(Csv, Options),
    ?assertEqual(Expected, Actual).

decode_tabbed_binary() ->
    Csv = <<"col1\tcol2\tcol3\r"
            "v1\t\"v2\"\tv,a,l,u,e,3">>,
    Expected = [["col1", "col2", "col3"],
                ["v1", "v2", "v,a,l,u,e,3"]],
    Options = [{delimiter, tab}],
    Actual = csv:decode_binary(Csv, Options),
    ?assertEqual(Expected, Actual).

decode_selected_columns() ->
    Data = generate_data(3, 1000),
    ColumnCapture = [3, 3, 2],
    FolderMaker =
        fun(Header1, Header2) ->
                ?assertEqual([<<"r1c1">>, <<"r1c2">>, <<"r1c3">>], Header1),
                ?assertEqual([<<"r2c1">>, <<"r2c2">>, <<"r2c3">>], Header2),
                {fun (Row, Acc) -> [Row | Acc] end,
                 ColumnCapture}
        end,
    Decoded = csv:decode_fold({maker, FolderMaker},
                              [],
                              {fun chunk_generator/1, encode_csv(Data)},
                              [{return, binary}]),
    [_, _ | DataBody] = Data,
    Expected = pluck(ColumnCapture, DataBody),
    ?assertEqual(Expected, lists:reverse(Decoded)).

decode_out_of_bounds_capture() ->
    Csv = <<"h1,h2,h3\nv1,v2,v3\nv11,v22,v33">>,
    ColumnCapture = [4, 2, 2, 5, 10000],
    FolderMaker =
        fun(["h1", "h2", "h3"]) ->
                {fun (Row, Acc) -> [Row | Acc] end,
                 ColumnCapture}
        end,
    Decoded = lists:reverse(csv:decode_fold({maker, FolderMaker},
                                            [],
                                            {fun chunk_generator/1, Csv},
                                            [{return, list}])),
    Expected = [["", "v2", "v2", "", ""],
                ["", "v22", "v22", "", ""]],
    ?assertEqual(Expected, Decoded).


%% Decode a CSV of an extreme amout of columns per kB.
decode_commas_only() ->
    Csv = repeat(<<",">>, 5000),
    Expected = [lists:duplicate(5001, "")],
    ?assertEqual(Expected, csv:decode_binary(Csv)).

%% Decode a CSV of an extrem amount of rows per kB. Empty lines are
%% stripped by libcsv since it's not runnning in strict mode.
decode_max_rows() ->
    Csv = repeat(<<"a\n">>, 5000),
    Expected = lists:duplicate(5000, ["a"]),
    ?assertEqual(Expected, csv:decode_binary(Csv)).

%% Utilities

generate_data(Cols, Rows) ->
    [generate_row(RowI, Cols) || RowI <- lists:seq(1, Rows)].

generate_row(RowI, Cols) ->
    [iolist_to_binary(["r", integer_to_list(RowI), "c", integer_to_list(ColI)])
     || ColI <- lists:seq(1, Cols)].

encode_csv(Matrix) ->
    iolist_to_binary([encode_csv_row(Row) || Row <- Matrix]).

encode_csv_row(Row) ->
    encode_csv_row(Row, []).

encode_csv_row([], Acc) ->
    lists:reverse(Acc);
encode_csv_row([Col], Acc) ->
    NewAcc = [["\"", Col, "\"\n"] | Acc],
    encode_csv_row([], NewAcc);
encode_csv_row([Col | Rest], Acc) ->
    NewAcc = [["\"", Col, "\","] | Acc],
    encode_csv_row(Rest, NewAcc).

chunk_generator(<<>>) ->
    {<<>>, done};
chunk_generator(Csv) ->
    ChunkSize = rand:uniform(8) - 1,
    csv_binary:split_by_size(Csv, ChunkSize).

pluck(Columns, Rows) ->
    pluck(Columns, Rows, []).

pluck(_, [], Acc) ->
    lists:reverse(Acc);
pluck(Columns, [Row | Rest], Acc) ->
    Plucked = [lists:nth(ColI, Row) || ColI <- Columns],
    NewAcc = [Plucked | Acc],
    pluck(Columns, Rest, NewAcc).

repeat(Bin, N) ->
    iolist_to_binary(lists:duplicate(N, Bin)).
