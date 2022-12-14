-module(files).

-include("../../include/dbc.hrl").

-export([
    make_folder/1,
    make_index_folder/0,
    read/1,
    write/2,
    create/1
]).

-spec make_folder(File::binary()) -> 'ok' | {error, atom()}.
-spec make_index_folder() -> 'ok' | {error, atom()}.
-spec read(File::binary()) -> binary().
-spec write(File::binary(), Data::binary()) -> 'ok' | {error, _Reason}.
-spec create(File::binary()) -> string().

make_folder(File) ->
    logger:debug("make_folder File ~p", [File]),
    FileName = erlang:binary_to_list(File),
    case file:make_dir(FileName) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} -> {error, Reason}
    end.

make_index_folder() ->
    logger:debug("make_index_folder"),
    case file:make_dir(?INDEX_FOLDER) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} -> {error, Reason}
    end. 

read(File) ->
    logger:debug("read File ~p", [File]),
    case file:read_file(?DBC_FOLDER ++
                   "/" ++
                   erlang:binary_to_list(File)) of
        {ok, Data}      -> Data;
        {error, Reason} -> Reason
    end.

write(File, Data) ->
    logger:debug("write File ~p Data ~p", [File, Data]),
    ok = file:write_file(?DBC_FOLDER ++
                         "/" ++
                         erlang:binary_to_list(File), [Data]).

create(File) ->
    logger:debug("create File ~p", [File]),
    FileName = erlang:binary_to_list(File),
    {ok, IoDevice} = file:open(FileName, [write]),
    ok = file:close(IoDevice),
    FileName.
