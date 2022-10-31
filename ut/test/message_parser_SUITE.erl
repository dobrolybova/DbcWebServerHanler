-module(message_parser_SUITE).

-include("include.hrl").

-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         prepare_msg_ok_test/1,
         prepare_msg_wrong_file_not_binary_test/1,
         prepare_msg_wrong_file_int_test/1,
         prepare_msg_wrong_dir_bin_test/1,
         prepare_msg_wrong_dir_int_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        prepare_msg_ok_test,
        prepare_msg_wrong_file_not_binary_test,
        prepare_msg_wrong_file_int_test,
        prepare_msg_wrong_dir_bin_test,
        prepare_msg_wrong_dir_int_test
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    meck:new(json_wrapper),
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, open, fun ((?TEST_DIRECTORY ++_String), [write]) -> {ok, <<>>} end),
    meck:expect(file, close, fun(_IoDevice) -> ok end),
    meck:expect(json_wrapper, get_messages_ids, fun(?TEST_FILE) -> ?TEST_IDS end),
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

prepare_msg_ok_test(_Config) ->
    Res = messages_parser:prepare_msg(?TEST_DIRECTORY, erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual(ok, Res).

prepare_msg_wrong_file_not_binary_test(_Config) ->
    try messages_parser:prepare_msg(?TEST_DIRECTORY, ?TEST_FILE),
        ?assert(false)
    catch error:Error ->
        ?assertEqual(badarg, Error)
    end.

prepare_msg_wrong_file_int_test(_Config) ->
    try messages_parser:prepare_msg(1, ?TEST_FILE),
        ?assert(false)
    catch error:Error ->
        ?assertEqual(badarg, Error)
    end.

prepare_msg_wrong_dir_bin_test(_Config) ->
    try messages_parser:prepare_msg(erlang:list_to_binary(?TEST_DIRECTORY), ?TEST_FILE),
        ?assert(false)
    catch error:Error ->
        ?assertEqual(badarg, Error)
    end.

prepare_msg_wrong_dir_int_test(_Config) ->
    try ok = messages_parser:prepare_msg(1, ?TEST_FILE),
        ?assert(false)
    catch error:Error ->
        ?assertEqual(badarg, Error)
    end.

