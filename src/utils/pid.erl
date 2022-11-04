-module(pid).

-export([
    update/2,
    get/1
]).

-spec update(Filename::binary(), Pid::pid()) ->  ok.
-spec get(Filename::binary()) -> any().

update(Filename, Pid) ->
    FileKey = hash:make_hash(Filename),
    persistent_term:put(FileKey, Pid).

get(Filename) ->
    FileKey = hash:make_hash(Filename),
    persistent_term:get(FileKey).