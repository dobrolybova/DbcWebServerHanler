-module(message_SUITE).

-include("defines.hrl").

-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         prepare_msg_ok_test/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        prepare_msg_ok_test
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    meck:new(json),
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, open, fun ((?TEST_DIRECTORY ++_String), [write]) -> {ok, <<>>} end),
    meck:expect(file, close, fun(_IoDevice) -> ok end),
    meck:expect(json, get_messages_ids, fun(?TEST_FILE) -> ?TEST_IDS end),
    meck:expect(json, write, fun(_Data, ?TEST_DIRECTORY, _File) -> 'ok' end),
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

prepare_msg_ok_test(_Config) ->
    Res = messages:prepare_msg(?TEST_DIRECTORY, erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual(ok, Res).
