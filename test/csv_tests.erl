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
       ?_test(decode_selected_columns())}]}.

setup() ->
    ok.

teardown(ok) ->
    ok.

decode_binary() ->
    Csv = <<"col1,col2,\"col with a \"\"\",col4\nonly column in row 2">>,
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
    Csv = <<"col1\tcol2\tcol3\rv1\t\"v2\"\tv,a,l,u,e,3">>,
    Expected = [["col1", "col2", "col3"],
                ["v1", "v2", "v,a,l,u,e,3"]],
    Options = [{delimiter, tab}],
    Actual = csv:decode_binary(Csv, Options),
    ?assertEqual(Expected, Actual).

decode_selected_columns() ->
    Csv = <<"h1v1,h1v2,h1v3\n"
            "h2v1,h2v2,h2v3\n"
            "r1v1,r1v2,r1v3\n",
            "r2v1,r2v2,r2v3">>,
    Expected = [["r1v3", "r1v3", "r1v1"],
                ["r2v3", "r2v3", "r2v1"]],
    FolderMaker = fun(Header1, Header2) ->
                          ?assertEqual(["h1v1", "h1v2", "h1v3"], Header1),
                          ?assertEqual(["h2v1", "h2v2", "h2v3"], Header2),
                          {fun (Row, Acc) -> [Row | Acc] end,
                           [2, 2, 0]}
                  end,
    Actual = lists:reverse(
               csv:decode_binary_fold({maker, FolderMaker}, [], Csv)),
    ?assertEqual(Expected, Actual).
