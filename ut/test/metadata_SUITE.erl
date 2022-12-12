-module(metadata_SUITE).

-include("defines.hrl").

-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         prepare_metadata_ok_test/1,
         update_status_ok_test/1,
         update_status_nok_test/1,
         get_status_ok_test/1,
         create_file_ok_test/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        prepare_metadata_ok_test,
        update_status_ok_test,
        update_status_nok_test,
        get_status_ok_test,
        create_file_ok_test
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, read_file_info, fun ("dbc/" ++ ?TEST_FILE) -> 
        {ok, 
            {file_info, 256, "", "", 
                {{"","",""},{"","",""}},
                {{"","",""},{"","",""}},
                {{"","",""},{"","",""}}
                ,"","","","","","",""
            }
        } end),
    meck:expect(file, write_file, fun(_Directory, _Data) -> ok end),
    meck:expect(file, read_file, fun(_Filename) -> {ok, <<"{\"status\": \"Unknown\"}">>} end),
    meck:expect(file, open, fun ((?TEST_DIRECTORY ++_String), [write]) -> {ok, <<>>} end),
    meck:expect(file, close, fun(_IoDevice) -> ok end),
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

prepare_metadata_ok_test(_Config) ->
    Res = metadata:prepare_metadata(?TEST_DIRECTORY, erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual(ok, Res).

update_status_ok_test(_Config) ->
    Res = metadata:update_status("Done", ?TEST_DIRECTORY, erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual(ok, Res).

update_status_nok_test(_Config) ->
    meck:expect(file, write_file, fun(_Directory, _Data) -> {error, "Error"} end),
    Res = metadata:update_status("Done", ?TEST_DIRECTORY, erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual({error, "Error"}, Res).

get_status_ok_test(_Config) ->
    Res = metadata:get_status(erlang:list_to_binary(?TEST_FILE)),
    ?assertEqual("Unknown", Res).

create_file_ok_test(_Config) ->
    Res = metadata:create_file(?TEST_DIRECTORY),
    ?assertEqual(ok, Res).
