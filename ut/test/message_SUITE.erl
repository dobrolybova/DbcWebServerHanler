-module(message_SUITE).

-include("defines.hrl").

-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         prepare_msg_ok_test/1,
         get_message_id_ok_test/1,
         get_message_id_test/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        prepare_msg_ok_test,
        get_message_id_ok_test,
        get_message_id_test
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    meck:new(json),
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, open, fun(_Filename, [write]) -> {ok, <<>>} end),
    meck:expect(file, close, fun(_IoDevice) -> ok end),
    meck:expect(file, read_file, fun(_FileName) -> {ok, <<"BO_ 1 POWERTRAIN_DATA: 8 PCM">>} end),
    meck:expect(json, get_messages_ids, fun("dbc/index/" ++ ?TEST_FILE) -> ?TEST_IDS end),    
    meck:expect(json, write, fun(_Data, _Directory, _File) -> 'ok' end),
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

prepare_msg_ok_test(_Config) ->
    Res = messages:prepare_msg("dbc/index/ "++ ?TEST_DIRECTORY, erlang:list_to_binary("dbc/index/" ++ ?TEST_FILE)),
    ?assertEqual(ok, Res).

get_message_id_ok_test(_Config) ->
    Res = messages:get_message_id(["Some string"]),
    ?assertEqual(ok, Res).

get_message_id_test(_Config) ->
    Res = messages:get_message_id(["BO_", "1", "POWERTRAIN_DATA:", "8", "PCM"]),
    ?assertEqual(1, Res).
