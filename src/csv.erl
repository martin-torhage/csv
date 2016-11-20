-module(csv).
-export([decode_fold/3,
         decode_binary_fold/3,
         decode_binary/1,
         decode_gzip_fold/3]).

-define(GZIP_HEADER_SIZE, 31).

-record(state,
        {parser          :: term(),
         parser_state    :: has_csv | eob,
         generator       :: fun(),
         generator_state :: term()}).

decode_fold(Folder, AccIn, {Generator, GeneratorState}) ->
    State = #state{generator = Generator,
                   generator_state = GeneratorState},
    decode_fold1(Folder, AccIn, State).

decode_binary_fold(Folder, AccIn, Csv) when is_binary(Csv) ->
    Generator = fun(init_state) ->
                        {Csv, done}
                end,
    decode_fold(Folder, AccIn, {Generator, init_state}).

decode_binary(Csv) when is_binary(Csv) ->
    Folder = fun(Row, Acc) -> [Row | Acc] end,
    lists:reverse(decode_binary_fold(Folder, [], Csv)).

decode_gzip_fold(Folder, AccIn, CsvGzip) when is_binary(CsvGzip) ->
    Z = zlib:open(),
    ok = zlib:inflateInit(Z, ?GZIP_HEADER_SIZE),
    Generator =
        fun(Gzip) ->
                {GzipHead, GzipRest} = binary_split_by_size(Gzip, 16 * 1024),
                case zlib:inflate(Z, GzipHead) of
                    [] when GzipRest =:= <<>> ->
                        ok = zlib:inflateEnd(Z),
                        ok = zlib:close(Z),
                        {<<>>, done};
                    IoList ->
                        {iolist_to_binary(IoList), GzipRest}
                end
        end,
    decode_fold(Folder, AccIn, {Generator, CsvGzip}).

%% Internal

decode_fold1(Folder, Acc, #state{parser = undefined} = State) ->
    {ok, Parser} = csv_parser:init(),
    NewState = State#state{parser = Parser,
                           parser_state = eob},
    decode_fold1(Folder, Acc, NewState);
decode_fold1(Folder, Acc, #state{generator_state = done,
                                 parser_state = eob} = State) ->
    {ok, Rows} = csv_parser:close(State#state.parser),
    lists:foldl(Folder, Acc, Rows);
decode_fold1(Folder, Acc, #state{parser_state = eob} = State) ->
    decode_fold1(Folder, Acc, feed_nif(State));
decode_fold1(Folder, Acc, State) ->
    {NewAcc, NewState} = case csv_parser:parse(State#state.parser) of
                             {error, eob} ->
                                 {Acc, State#state{parser_state = eob}};
                             {ok, Rows} ->
                                 {lists:foldl(Folder, Acc, Rows), State}
                         end,
    decode_fold1(Folder, NewAcc, NewState).

binary_split_by_size(Bin, Size) ->
    case Size >= byte_size(Bin) of
        true ->
            {Bin, <<>>};
        false ->
            {binary:part(Bin, 0, Size),
             binary:part(Bin, Size, byte_size(Bin) - Size)}
    end.

feed_nif(State) ->
    #state{generator = Generator,
           generator_state = GeneratorState,
           parser = Parser} = State,
    {CsvChunk, NewGeneratorState} = Generator(GeneratorState),
    ok = csv_parser:feed(Parser, CsvChunk),
    State#state{generator_state = NewGeneratorState,
                parser_state = has_csv}.
