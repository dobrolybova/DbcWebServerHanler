- module(parser).

-export([
    start_parser_process/2,
    parser/2
]).

-include("../../include/dbc.hrl").

-spec start_parser_process(Filename::binary(), Data::binary()) -> 'ok'.

create_data_files(Directory) ->
    metadata:create_file(Directory),
    index:create_file(Directory).

create_foldef_structure(FileName) ->
    ok = files:make_index_folder(),
    Directory = ?INDEX_FOLDER ++ "/" ++ erlang:binary_to_list(FileName),
    ok = files:make_folder(erlang:list_to_binary(Directory)),
    ok = create_data_files(Directory),
    Directory.

parse(FileName) ->
    Directory = create_foldef_structure(FileName),
    ok = metadata:prepare_metadata(Directory, FileName),
    ok = index:prepare_index(Directory, FileName),
    ok = messages:prepare_msg(Directory, FileName),
    ok = metadata:update_status(?DONE, Directory, FileName).

is_parsing_needed(File, FileHash, DataHash) ->
    Res = hash:compare(FileHash, DataHash),
    MetaStatus = case index:is_exist(File) of
                    true  -> metadata:get_status(File);
                    false -> ?UNKNOWN
                    end,
    logger:notice("is_parsing_needed Res ~p MetaStatus ~p", [Res, MetaStatus]),
    case {Res, MetaStatus} of
        {true,?DONE} -> false;        
        {_ ,_}        -> true
    end.

parser(Filename, Data) ->
    logger:notice("handle"),
    FillData = case Data of
                    <<>> -> files:read(Filename);
                    _    -> Data
                end,
    DataHash = hash:make_hash(FillData),
    FileHash = hash:get_file_hash(Filename),
    case is_parsing_needed(Filename, FileHash, DataHash) of
        true -> 
            logger:notice("Update"),
            hash:update(Filename, DataHash),
            files:write(Filename, FillData),
            parse(Filename);
        false -> ok
    end.

start_parser_process(Filename, Data) ->
    Pid = spawn(parser, parser, [Filename, Data]),
    logger:notice("Put file ~p with pid ~p", [Filename, Pid]),
    pid:update(Filename, Pid).
