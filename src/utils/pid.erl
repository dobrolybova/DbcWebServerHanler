-module(pid).

-export([
    update/2,
    get/1
]).

-spec update(FileName::binary(), Pid::pid()) ->  ok.
-spec get(FileName::binary()) -> any().

update(FileName, Pid) ->
    logger:debug("update pid FileName ~p pid ~p", [FileName, Pid]),
    FileKey = hash:make_hash(FileName),
    persistent_term:put(FileKey, Pid).

get(FileName) ->
    logger:debug("FileName pid for FileName ~p", [FileName]),
    FileKey = hash:make_hash(FileName),
    persistent_term:get(FileKey).