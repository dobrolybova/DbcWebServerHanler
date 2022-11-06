-module(metadata).

-include("../../include/dbc.hrl").

-export([
    prepare_metadata/2,
    update_status/3,
    get_status/1,
    create_file/1
]).

-spec prepare_metadata(Directory::string(), FileName::binary()) -> 'ok' | {'error', atom()}.
-spec update_status(Status::string(), Directory::string(), FileName::binary()) -> 'ok' | {'error', atom()}.
-spec get_status(FileName::binary()) -> string().
-spec create_file(Directory::string()) -> 'ok'.

prepare_metadata(Directory, FileName) ->
    logger:debug("prepare_metadata Directory ~p FileName ~p", [Directory, FileName]),
    {ok, 
        {file_info, Size, _, _, 
            {{_,_,_},{_,_,_}},
            {{_,_,_},{_,_,_}},
            {{_,_,_},{_,_,_}}
            ,_,_,_,_,_,_,_
        }
    } = file:read_file_info(?DBC_FOLDER ++ "/" ++ binary_to_list(FileName)),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(os:timestamp()),
    StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
    Data = #{fileName => FileName, fileSize => Size, uploadTimestamp => list_to_binary(StrTime), status => list_to_binary(?IN_PROGRESS)},
    json:write(Data, Directory, ?METADATA_FILE).

update_status(Status, Directory, FileName) ->
    logger:debug("update_status ~p Directory ~p FileName ~p", [Status, Directory, FileName]),
    File = erlang:binary_to_list(FileName),
    MapData = maps:update(<<"status">>, list_to_binary(Status), json:read_meta(File)),
    json:write(MapData, Directory, ?METADATA_FILE).

get_status(FileName) ->
    File = erlang:binary_to_list(FileName),
    MapData = json:read_meta(File),
    Status = maps:get(<<"status">>, MapData),
    logger:debug("get_status FileName ~p Status ~p", [FileName, Status]),
    erlang:binary_to_list(Status).

create_file(Directory) ->
    logger:debug("create_file Directory ~p", [Directory]),
    File = erlang:list_to_binary(Directory ++ "/" ++ ?METADATA_FILE), 
    files:create(File).   