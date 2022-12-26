-module(parser_SUITE).

-include("defines.hrl").

-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         start_parser_ok_test/1,
         parser_not_needed_test/1,
         parser_is_needed_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        start_parser_ok_test,
        parser_not_needed_test,
        parser_is_needed_test
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    % meck:new(json),
    meck:new(file, [unstick, passthrough]),
    % meck:expect(json, write, fun(_Data, _Directory, _IndexFile) -> ok end),
    meck:expect(file, read_file, fun(_Filename) -> {ok, <<"{\"status\": \"Done\"}">>} end),
    meck:expect(file, open, fun(_Filename, [write]) -> {ok, <<>>} end),
    meck:expect(file, close, fun(_IoDevice) -> ok end),
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

start_parser_ok_test(_Config) ->
    Data = "BO_ 304 GAS_PEDAL_2: 8 PCM",
    ok = parser:start_parser_process(erlang:list_to_binary(?TEST_FILE), Data).

parser_not_needed_test(_Config) ->
    Data = "BO_ 304 GAS_PEDAL_2: 8 PCM",
    ok = parser:parser(erlang:list_to_binary(?TEST_FILE), Data).

parser_is_needed_test(_Config) ->
    meck:expect(file, read_file, fun(_Filename) -> {ok, <<"{\"status\": \"Unknown\", \"msgIds\": [1, 2 , 3]}">>} end),
    Data = "BO_ 304 GAS_PEDAL_2: 8 PCM",
    ok = parser:parser(erlang:list_to_binary(?TEST_FILE), Data).

