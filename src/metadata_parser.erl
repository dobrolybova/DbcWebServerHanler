-module(metadata_parser).

-include("dbc.hrl").

-export([prepare_metadata/2]).

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
    Data = #{fileName => FileName, fileSize => Size, uploadTimestamp => list_to_binary(StrTime)},
    json_wrapper:write(Data, Directory, ?METADATA_FILE).