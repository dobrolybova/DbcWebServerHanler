-module(metadata_parser).

-include("dbc.hrl").

-export([prepare_metadata/2, update_status/3, get_status/1]).

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

update_status(Status, Directory, File) ->
    [FileName, _Extention] = string:split(erlang:binary_to_list(File), "."),
    MapData = maps:update(<<"status">>, list_to_binary(Status), json_wrapper:read_meta(FileName)),
    json_wrapper:write(MapData, Directory, ?METADATA_FILE).

get_status(File) ->
    [FileName, _Extention] = string:split(erlang:binary_to_list(File), "."),
    MapData = json_wrapper:read_meta(FileName),
    Status = maps:get(<<"status">>, MapData),
    erlang:binary_to_list(Status).
     