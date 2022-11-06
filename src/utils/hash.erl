-module(hash).

-include("../../include/dbc.hrl").

-export([
    clear_all/0,
    clear/1,
    compare/2,
    update/2,
    make_hash/1,
    get_file_hash/1
]).

-spec clear_all() -> 'ok'.
-spec clear(File::binary()) -> boolean().
-spec compare(FileHash::binary(), DataHash::binary()) -> true | false.
-spec update(FileName::binary(), Hash::'ok' | binary()) -> atom().
-spec make_hash(Data::binary()) -> binary().
-spec get_file_hash(FileName::binary()) -> binary().

make_hash(Data) ->
    logger:debug("make_hash Data ~p", [Data]),
    erlang:md5(Data).

get_file_hash(File) ->
    logger:debug("get_file_hash File ~p", [File]),
    try CurrHash = persistent_term:get(File),
        CurrHash
    catch error:Error -> Error        
    end.

clear_all() ->
    logger:debug("clear_all File"),
    {ok, FilesList} = file:list_dir(?DBC_FOLDER),
    [persistent_term:erase(FileName) || FileName <- lists:delete("index", FilesList)],
    ok.

clear(FileName) ->
    logger:debug("erase for file ~p", [FileName]),
    Res = persistent_term:erase(FileName),
    Res.

compare(FileHash, DataHash) ->
    logger:debug("compare FileHash ~p DataHash ~p", [FileHash, DataHash]),
    FileHash =:= DataHash.

update(FileName, Hash) ->
    logger:debug("put for file ~p, hash ~p", [FileName, Hash]),
    persistent_term:put(FileName, Hash).
