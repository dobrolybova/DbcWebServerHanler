- module(dbc_file_parser).

-export([parse/1]).

-include("dbc.hrl").

create_index_folder() ->
    case file:make_dir(?INDEX_FOLDER) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} -> {error, Reason}
    end.    

remove_file_ext(FileName) ->
    [File, _Extention] = string:split(FileName, "."),
    binary_to_list(File).

create_data_files(Directory) ->
    {ok, MetaIoDevice} = file:open(Directory ++ "/" ++ ?METADATA_FILE, [write]),
    ok = file:close(MetaIoDevice),
    {ok, IndexIoDevice} = file:open(Directory ++ "/" ++ ?INDEX_FILE, [write]),
    ok = file:close(IndexIoDevice),
    {ok, MsgIoDevice} = file:open(Directory ++ "/" ++ ?MSG_FILE, [write]),
    ok = file:close(MsgIoDevice).

create_filename_folder(FileName) ->
    File = remove_file_ext(FileName),
    Directory = ?INDEX_FOLDER ++ "/" ++ File,
    case file:make_dir(Directory) of
        ok -> {ok, Directory};
        {error, eexist} -> {ok, Directory};
        {error, Reason} -> {error, Reason}
    end.

create_foldef_structure(FileName) ->
    ok = create_index_folder(),
    {ok, Directory} = create_filename_folder(FileName),
    ok = create_data_files(Directory),
    Directory.

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
    FileData = jsone:encode(Data),
    ok = file:write_file(Directory ++ "/" ++ ?METADATA_FILE, [FileData]).


prepare_index(_Directory, _FileName) ->
    ok.

parse(FileName) ->
    Directory = create_foldef_structure(FileName),
    prepare_metadata(Directory, FileName),
    prepare_index(Directory, FileName).


