-module(csv_testbench).
-export([run/0, run/1]).

run() ->
    run("benchtest.csv").

run(Filename) ->
    {ok, Csv} = file:read_file(filename:join([code:priv_dir(csv), Filename])),

    FolderMaker = fun (_) ->
                          {fun(Row, Acc) -> [Row | Acc] end,
                           [5, 5, 1]}
                  end,
    Generator = make_generator(Csv),

    Timer = start_utimer(),

    %%Decoded = csv:decode_binary(Csv),
    Decoded = csv:decode_fold({maker, FolderMaker}, [], Generator),

    RunTimeUs = stop_utimer(Timer),
    #{run_time_ms => round(RunTimeUs / 1000),
      decoded_rows => length(Decoded),
      us_per_row => divide_and_round(RunTimeUs, length(Decoded)),
      rows_per_second => round(length(Decoded) / (RunTimeUs / 1000000)),
      first_row => hd(Decoded)}.

%% Internal

make_generator(Csv) ->
    {fun generator/1, Csv}.

generator(done) ->
    done;
generator(Csv) ->
    {Csv, done}.

start_utimer() ->
    erlang:system_time(micro_seconds).

stop_utimer(Timer) ->
    erlang:system_time(micro_seconds) - Timer.

divide_and_round(_, 0) ->
    infinity;
divide_and_round(Dividend, Divisor) ->
    round(Dividend / Divisor).
