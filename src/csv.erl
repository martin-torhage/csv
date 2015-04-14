-module(csv).
-export([decode_fold/3]).

decode_fold(Folder, AccIn, Csv) when is_binary(Csv) ->
    decode_fold1(Folder, AccIn, {undefined, Csv}).

% Internal

decode_fold1(Folder, Acc, {undefined, Csv}) ->
    Parser = csv_parser:init(),
    decode_fold1(Folder, Acc, {Parser, Csv});
decode_fold1(Folder, Acc, {Parser, <<>>}) ->
    Rows = csv_parser:close(Parser),
    lists:foldl(Folder, Acc, Rows);
decode_fold1(Folder, Acc, {Parser, Csv}) ->
    {CsvHead, CsvTail} = binary_split_by_size(Csv, 4096),
    NewAcc = lists:foldl(Folder, Acc, csv_parser:parse(Parser, CsvHead)),
    decode_fold1(Folder, NewAcc, {Parser, CsvTail}).

binary_split_by_size(Bin, Size) ->
    case Size >= byte_size(Bin) of
        true ->
            {Bin, <<>>};
        false ->
            {binary:part(Bin, 0, Size),
             binary:part(Bin, Size, byte_size(Bin) - Size)}
    end.
