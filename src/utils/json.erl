-module(json).

-export([
    write/3,
    read_meta/1,
    get_messages_ids/1
]).

-include("../../include/dbc.hrl").

-spec write(Data::map(), Directory::string(), File::string()) -> 'ok' | {'error', atom()}.
-spec read_meta(FileName::string()) -> map().
-spec get_messages_ids(FileName::string()) -> list().


write(Data, Directory, File) ->
    logger:debug("write json Data ~p Directory ~p File ~p", [Data, Directory, File]),
    FileData = jsone:encode(Data),
    file:write_file(Directory ++ "/" ++ File, [FileData]).

read_ix(FileName) ->
    logger:debug("read_ix FileName ~p", [FileName]),
    {ok, Data} = file:read_file(?DBC_FOLDER ++ "/" ++ "index/" ++ FileName ++ "/" ++ ?INDEX_FILE),
    jsone:decode(Data).  

read_meta(FileName) ->
    logger:debug("read_meta FileName ~p", [FileName]),
    {ok, Data} = file:read_file(?DBC_FOLDER ++ "/" ++ "index/" ++ FileName ++ "/" ++ ?METADATA_FILE),
    jsone:decode(Data).  

get_messages_ids(FileName) ->
    logger:debug("get_messages_ids FileName ~p", [FileName]),
    MapData = read_ix(FileName),
    maps:get(<<"msgIds">>, MapData).