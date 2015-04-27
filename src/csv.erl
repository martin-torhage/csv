-module(csv).
-export([decode_fold/3,
         decode_binary_fold/3,
         decode_gzip_fold/3]).

%% Max size of each batch to decode. This is also hardcoded in the
%% NIF.
-define(MAX_BATCH_SIZE, 4096).
-define(GZIP_HEADER_SIZE, 31).

-record(state,
        {parser          :: term(),
         generator       :: fun(),
         generator_state :: term(),
         csv_buffer      :: binary()}).

decode_fold(Folder, AccIn, {Generator, GeneratorState}) ->
    State = #state{generator = Generator,
                   generator_state = GeneratorState,
                   csv_buffer = <<>>},
    decode_fold1(Folder, AccIn, State).

decode_binary_fold(Folder, AccIn, Csv) when is_binary(Csv) ->
    Generator = fun(init_state) ->
                        {Csv, done}
                end,
    decode_fold(Folder, AccIn, {Generator, init_state}).

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
    NewState = State#state{parser = csv_parser:init()},
    decode_fold1(Folder, Acc, NewState);
decode_fold1(Folder, Acc,
             #state{generator_state = done, csv_buffer = <<>>} = State) ->
    Rows = csv_parser:close(State#state.parser),
    lists:foldl(Folder, Acc, Rows);
decode_fold1(Folder, Acc, #state{csv_buffer = CsvBuffer} = State)
  when CsvBuffer =/= <<>> ->
    {CsvHead, CsvTail} = binary_split_by_size(CsvBuffer, ?MAX_BATCH_SIZE),
    NewState = State#state{csv_buffer = CsvTail},
    Rows = csv_parser:parse(State#state.parser, CsvHead),
    NewAcc = lists:foldl(Folder, Acc, Rows),
    decode_fold1(Folder, NewAcc, NewState);
decode_fold1(Folder, Acc, State) ->
    #state{generator = Generator,
           generator_state = GeneratorState} = State,
    {NewCsvBuffer, NewGeneratorState} = Generator(GeneratorState),
    NewState = State#state{csv_buffer = NewCsvBuffer,
                           generator_state = NewGeneratorState},
    decode_fold1(Folder, Acc, NewState).

binary_split_by_size(Bin, Size) ->
    case Size >= byte_size(Bin) of
        true ->
            {Bin, <<>>};
        false ->
            {binary:part(Bin, 0, Size),
             binary:part(Bin, Size, byte_size(Bin) - Size)}
    end.
