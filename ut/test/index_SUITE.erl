-module(index_SUITE).

-include("defines.hrl").

-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         prepare_index_ok_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        prepare_index_ok_test
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    meck:new(json),
    meck:new(file, [unstick, passthrough]),
    meck:expect(json, write, fun(_Data, _Directory, _IndexFile) -> ok end),
    meck:expect(file, read_file, fun(_Filename) -> {ok, <<>>} end),
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

prepare_index_ok_test(_Config) ->
    ok = index:prepare_index(?TEST_DIRECTORY, erlang:list_to_binary(?TEST_FILE)).