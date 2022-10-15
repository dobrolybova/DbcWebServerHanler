-module(json_wrapper).

-export([write/3, read_meta/1, get_messages_ids/1]).

-include("dbc.hrl").

-spec write(Data::map(), Directory::string(), File::string()) -> 'ok' | {'error', atom()}.
-spec read_ix(FileName::string()) -> map().
-spec read_meta(FileName::string()) -> map().
-spec get_messages_ids(FileName::string()) -> list().


write(Data, Directory, File) ->
    FileData = jsone:encode(Data),
    file:write_file(Directory ++ "/" ++ File, [FileData]).

read_ix(FileName) ->
    {ok, Data} = file:read_file(?DBC_FOLDER ++ "/" ++ "index/" ++ FileName ++ "/" ++ ?INDEX_FILE),
    jsone:decode(Data).  

read_meta(FileName) ->
    {ok, Data} = file:read_file(?DBC_FOLDER ++ "/" ++ "index/" ++ FileName ++ "/" ++ ?METADATA_FILE),
    jsone:decode(Data).  

get_messages_ids(File) ->
    [FileName, _Extention] = string:split(File, "."),
    MapData = read_ix(FileName),
    maps:get(<<"msgIds">>, MapData).