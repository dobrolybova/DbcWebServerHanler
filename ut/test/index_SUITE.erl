-module(index_SUITE).

-include("defines.hrl").

-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         prepare_index_ok_test/1,
         is_exist_true_test/1,
         is_exist_false_empty_test/1,
         is_exist_false_error_test/1,
         create_file_ok_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        prepare_index_ok_test,
        is_exist_true_test,
        is_exist_false_empty_test,
        is_exist_false_error_test,
        create_file_ok_test
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    meck:new(json),
    meck:new(file, [unstick, passthrough]),
    meck:expect(json, write, fun(_Data, _Directory, _IndexFile) -> ok end),
    meck:expect(file, read_file, fun(_Filename) -> {ok, <<>>} end),
    meck:expect(file, open, fun(_Filename, [write]) -> {ok, <<>>} end),
    meck:expect(file, close, fun(_IoDevice) -> ok end),
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

prepare_index_ok_test(_Config) ->
    Res = index:prepare_index(?TEST_DIRECTORY, erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual(ok, Res).

is_exist_true_test(_Config) ->
    meck:expect(file, list_dir, fun(_Filename) -> {ok, "Data"} end),
    Res = index:is_exist(erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual(true, Res).

is_exist_false_empty_test(_Config) ->
    meck:expect(file, list_dir, fun(_Filename) -> {ok, []} end),
    Res = index:is_exist(erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual(false, Res).

is_exist_false_error_test(_Config) ->
    meck:expect(file, list_dir, fun(_Filename) -> {error, []} end),
    Res = index:is_exist(erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual(false, Res).

create_file_ok_test(_Config) ->
    Res = index:create_file(?TEST_DIRECTORY),
    ?assertEqual("TestDir/index.json", Res).