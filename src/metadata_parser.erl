-module(metadata_parser).

-include("dbc.hrl").

-export([prepare_metadata/2, update_status/3, get_status/1]).

-spec prepare_metadata(Directory::string(), FileName::binary()) -> 'ok' | {'error', atom()}.
-spec update_status(Status::string(), Directory::string(), FileName::binary()) -> 'ok' | {'error', atom()}.
-spec get_status(FileName::binary()) -> string().

prepare_metadata(Directory, FileName) ->
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
    json_wrapper:write(Data, Directory, ?METADATA_FILE).

update_status(Status, Directory, FileName) ->
    [File, _Extention] = string:split(erlang:binary_to_list(FileName), "."),
    MapData = maps:update(<<"status">>, list_to_binary(Status), json_wrapper:read_meta(File)),
    json_wrapper:write(MapData, Directory, ?METADATA_FILE).

get_status(FileName) ->
    [File, _Extention] = string:split(erlang:binary_to_list(FileName), "."),
    MapData = json_wrapper:read_meta(File),
    Status = maps:get(<<"status">>, MapData),
    erlang:binary_to_list(Status).
     