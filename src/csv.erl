-module(csv).
-export([decode_fold/3,
         decode_fold/4,
         decode_binary_fold/3,
         decode_binary_fold/4,
         decode_binary/1,
         decode_binary/2,
         decode_gzip_fold/3,
         decode_gzip_fold/4]).

-define(DEFAULT_OPTIONS, [{delimiter, comma},
                          {return, list}]).

-define(GZIP_HEADER_SIZE, 31).
-define(OPTION_TAB_DELIMITED, 1).
-define(OPTION_COMMA_DELIMITED, 0).
-define(OPTION_RETURN_BINARY, 2).
-define(OPTION_RETURN_LIST, 0).

-record(state,
        {parser          :: term(),
         parser_state    :: has_csv | eob,
         generator       :: fun(),
         generator_state :: term(),
         options         :: list()}).

decode_fold(Folder, AccIn, Generator) ->
    decode_fold(Folder, AccIn, Generator, ?DEFAULT_OPTIONS).

decode_fold({maker, FolderMaker}, AccIn,
            {Generator, GeneratorState}, Options) ->
    {arity, Arity} = erlang:fun_info(FolderMaker, arity),
    State = #state{generator = Generator,
                   generator_state = GeneratorState,
                   options = Options},
    case decode_n_rows(State, Arity) of
        not_enough_rows ->
            AccIn;
        {Rows, NewState} ->
            {Folder, Capture} = erlang:apply(FolderMaker, Rows),
            ok = csv_parser:set_capture(NewState#state.parser, Capture),
            decode_fold1(Folder, AccIn, NewState)
    end;
decode_fold(Folder, AccIn, {Generator, GeneratorState}, Options) ->
    State = #state{generator = Generator,
                   generator_state = GeneratorState,
                   options = Options},
    decode_fold1(Folder, AccIn, State).

decode_binary_fold(Folder, AccIn, Csv) ->
    decode_binary_fold(Folder, AccIn, Csv, ?DEFAULT_OPTIONS).

decode_binary_fold(Folder, AccIn, Csv, Options) when is_binary(Csv) ->
    Generator = fun(init_state) ->
                        {Csv, done}
                end,
    decode_fold(Folder, AccIn, {Generator, init_state}, Options).

decode_binary(Csv) ->
    decode_binary(Csv, ?DEFAULT_OPTIONS).

decode_binary(Csv, Options) when is_binary(Csv) ->
    Folder = fun(Row, Acc) -> [Row | Acc] end,
    lists:reverse(decode_binary_fold(Folder, [], Csv, Options)).

decode_gzip_fold(Folder, AccIn, CsvGzip) ->
    decode_gzip_fold(Folder, AccIn, CsvGzip, ?DEFAULT_OPTIONS).

decode_gzip_fold(Folder, AccIn, CsvGzip, Options) when is_binary(CsvGzip) ->
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
    decode_fold(Folder, AccIn, {Generator, CsvGzip}, Options).

%% Internal

decode_n_rows(State, N) ->
    decode_n_rows(State, N, []).

decode_n_rows(State, 0, Acc) ->
    {lists:reverse(Acc), State};
decode_n_rows(State, N, Acc) ->
    case parse(fun csv_parser:parse_one_row/1, State) of
        done ->
            not_enough_rows;
        {[Row], NewState}->
            decode_n_rows(NewState, N - 1, [Row | Acc])
    end.

parse(_, #state{parser = closed}) ->
    done;
parse(ParseFun, #state{parser = undefined, options = Options} = State) ->
    NewState = State#state{parser = init_parser(Options),
                           parser_state = eob},
    parse(ParseFun, NewState);
parse(_, #state{generator_state = done,
                parser_state = eob} = State) ->
    {ok, Rows} = csv_parser:close(State#state.parser),
    NewState = State#state{parser = closed},
    {Rows, NewState};
parse(ParseFun, #state{parser_state = eob} = State) ->
    parse(ParseFun, feed_nif(State));
parse(ParseFun, State) ->
    case ParseFun(State#state.parser) of
        {error, eob} ->
            parse(ParseFun, State#state{parser_state = eob});
        {ok, Rows} ->
            {Rows, State}
    end.

init_parser(Options) ->
    {ok, Parser} = csv_parser:init(parser_options(Options)),
    Parser.

parser_options(Options) ->
    Folder = fun(Option, Acc) ->
                     Acc + parser_option(Option)
             end,
    lists:foldl(Folder, 0, Options).

parser_option({delimiter, comma}) ->
    ?OPTION_COMMA_DELIMITED;
parser_option({delimiter, tab}) ->
    ?OPTION_TAB_DELIMITED;
parser_option({return, binary}) ->
    ?OPTION_RETURN_BINARY;
parser_option({return, list}) ->
    ?OPTION_RETURN_LIST.

decode_fold1(Folder, Acc, State) ->
    case parse(fun csv_parser:parse/1, State) of
        done ->
            Acc;
        {Rows, NewState} ->
            NewAcc = lists:foldl(Folder, Acc, Rows),
            decode_fold1(Folder, NewAcc, NewState)
    end.

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
