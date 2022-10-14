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
    ok = file:close(IndexIoDevice).

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

parse(FileName) ->
    Directory = create_foldef_structure(FileName),
    ok = metadata_parser:prepare_metadata(Directory, FileName),
    ok = index_parser:prepare_index(Directory, FileName),
    ok = messages_parser:prepare_msg(Directory, FileName),
    ok = metadata_parser:update_status(?DONE, Directory, FileName).


