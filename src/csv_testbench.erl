-module(csv_testbench).
-export([run/0, run/1]).

run() ->
    run("benchtest.csv").

run(Filename) ->
    {ok, Csv} = file:read_file(filename:join([code:priv_dir(csv), Filename])),
    Timer = start_utimer(),
    Decoded = csv:decode_binary(Csv),

    RunTimeUs = stop_utimer(Timer),
    #{run_time_ms => round(RunTimeUs / 1000),
      decoded_rows => length(Decoded),
      us_per_row => divide_and_round(RunTimeUs, length(Decoded))}.

%% Internal

start_utimer() ->
    erlang:system_time(micro_seconds).

stop_utimer(Timer) ->
    erlang:system_time(micro_seconds) - Timer.

divide_and_round(_, 0) ->
    infinity;
divide_and_round(Dividend, Divisor) ->
    round(Dividend / Divisor).
