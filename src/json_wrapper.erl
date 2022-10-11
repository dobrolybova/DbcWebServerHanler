-module(json_wrapper).

-export([write/3, get_messages_ids/1]).

-include("dbc.hrl").

write(Data, Directory, File) ->
    FileData = jsone:encode(Data),
    file:write_file(Directory ++ "/" ++ File, [FileData]).

get_messages_ids(File) ->
    [FileName, _Extention] = string:split(File, "."),
    {ok, Data} = file:read_file(?DBC_FOLDER ++ "/" ++ "index/" ++ FileName ++ "/" ++ ?INDEX_FILE),
    MapData = jsone:decode(Data),
    maps:get(<<"msgIds">>, MapData).