-module(csv_tests).

-include_lib("eunit/include/eunit.hrl").

csv_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"Should decode binary",
       ?_test(decode_binary())}]}.

setup() ->
    ok.

teardown(ok) ->
    ok.

decode_binary() ->
    Csv = <<"col1,col2,\"col with a \"\"\",col4\nonly column in row 2">>,
    Expected = [["col1", "col2", "col with a \"", "col4"],
                ["only column in row 2"]],
    Folder = fun(Row, Acc) ->
                     [Row | Acc]
             end,
    Actual = lists:reverse(csv:decode_binary_fold(Folder, [], Csv)),
    ?assertEqual(Expected, Actual).
