-module(csv).
-export([decode_fold/3,
         decode_binary_fold/3,
         decode_binary/1,
         decode_gzip_fold/3]).

%% Max size of each CSV batch to decode. This is also hardcoded in the
%% NIF.
-define(MAX_BATCH_SIZE, 4096).
-define(GZIP_HEADER_SIZE, 31).

-record(state,
        {parser          :: term(),
         generator       :: fun(),
         generator_state :: term(),
         csv_buffer      :: binary(),
         parsed_buffer   :: [[string()]]}).

decode_fold(Folder, AccIn, {Generator, GeneratorState}) ->
    State = #state{generator = Generator,
                   generator_state = GeneratorState,
                   csv_buffer = <<>>,
                   parsed_buffer = []},
    {complete, Decoded} = decode_fold1(Folder, AccIn, State),
    Decoded.

decode_binary_fold(Folder, AccIn, Csv) when is_binary(Csv) ->
    Generator = fun(init_state) ->
                        {Csv, done}
                end,
    CtrlFolder = fun (Row, CtrlAcc) ->
                         {ok, Folder(Row, CtrlAcc)}
                 end,
    decode_fold(CtrlFolder, AccIn, {Generator, init_state}).

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
    NewState = State#state{parser = Parser},
    decode_fold1(Folder, Acc, NewState);
decode_fold1(Folder, Acc, #state{parsed_buffer = ParsedRows} = State)
  when ParsedRows =/= [] ->
    CtrlFolder = fun(Row, {Acc2}) ->
                         case Folder(Row, Acc2) of
                             {ok, NewAcc2} ->
                                 {NewAcc2};
                             {pause, NewAcc2} ->
                                 {NewAcc2, []}
                         end;
                    (Row, {Acc2, RestBuffer}) ->
                         {Acc2, [Row | RestBuffer]}
                 end,
    case lists:foldl(CtrlFolder, {Acc}, ParsedRows) of
        {NewAcc} ->
            NewState = State#state{parsed_buffer = []},
            decode_fold1(Folder, NewAcc, NewState);
        {NewAcc, RestRows} ->
            NewState = State#state{parsed_buffer = RestRows},
            {paused, NewAcc, NewState}
    end;
decode_fold1(_, Acc, #state{generator_state = done,
                                 csv_buffer = <<>>,
                                 parser = closed}) ->
    {complete, Acc};
decode_fold1(Folder, Acc, #state{generator_state = done,
                                 csv_buffer = <<>>,
                                 parsed_buffer = []} = State) ->
    {ok, Rows} = csv_parser:close(State#state.parser),
    NewState = State#state{parser = closed,
                           parsed_buffer = Rows},
    decode_fold1(Folder, Acc, NewState);
decode_fold1(Folder, Acc, #state{csv_buffer = CsvBuffer,
                                 parsed_buffer = []} = State)
  when CsvBuffer =/= <<>> ->
    {CsvHead, CsvTail} = binary_split_by_size(CsvBuffer, ?MAX_BATCH_SIZE),
    {ok, Rows} = csv_parser:parse(State#state.parser, CsvHead),
    NewState = State#state{csv_buffer = CsvTail,
                           parsed_buffer = Rows},
    decode_fold1(Folder, Acc, NewState);
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
