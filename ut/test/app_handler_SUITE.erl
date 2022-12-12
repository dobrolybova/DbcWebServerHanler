-module(app_handler_SUITE).

-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         get_dbc_not_loaded_test/1,
         post_dbc_empty_test/1,
         get_dbc_wrong_file_test/1,
         post_dbc_wrong_file_test/1,
         post_dbc_ok_test/1,
         hello_test/1,
         handle_hash_nok_test/1,
         handle_hash_ok_test/1,
         handle_hash_file_nok_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        get_dbc_not_loaded_test,
        post_dbc_empty_test,
        get_dbc_wrong_file_test,
        post_dbc_wrong_file_test,
        post_dbc_ok_test,
        hello_test,
        handle_hash_nok_test,
        handle_hash_ok_test,
        handle_hash_file_nok_test
    ].

init_per_suite(Config) ->
    % meck:new(log),
    % meck:expect(log, add_handlers, fun() -> ok end),
    % meck:expect(log, set_level, fun(_Level) -> ok end),
    {ok, App_Start_List} = start([webserver]),
    inets:start(),
    [{app_start_list, App_Start_List}|Config].

init_per_testcase(_, Config) ->
    meck:new(file, [unstick]),
    meck:expect(file, read_file, fun(_Filename) -> {ok, <<>>} end),
    meck:expect(file, write_file, fun(_Filename, [_Data]) -> ok end),
    meck:expect(file, open, fun ((_Dir), [write]) -> {ok, <<>>} end),
    meck:expect(file, close, fun(_IoDevice) -> ok end),
    meck:expect(file, make_dir, fun(_Filename) -> ok end),
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    inets:stop(),
    stop(?config(app_start_list, Config)),
    Config.

start(Apps) ->
    {ok, do_start(_To_start = Apps, _Started = [])}.

do_start([], Started) ->
    Started;
do_start([App|Apps], Started) ->
    case application:start(App) of
    ok ->
        do_start(Apps, [App|Started]);
    {error, {not_started, Dep}} ->
        do_start([Dep|[App|Apps]], Started)
    end.

stop(Apps) ->
    _ = [ application:stop(App) || App <- Apps ],
    ok.

get_dbc_not_loaded_test(_Config) ->
    {ok, {{_Version, 404, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080/api/dbc?filename=acura_rdx_2020_can_generated.dbc", []}, [], []),
    Body == "The file was not loaded\n".

get_dbc_wrong_file_test(_Config) ->
    {ok, {{_Version, 404, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080/api/dbc?filename=acura_rdx_2020_can_generated", []}, [], []),
    Body == "The file was not loaded\n".

post_dbc_empty_test(_Config) ->
    {ok, {{_Version, 400, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(post, {"http://localhost:8080/api/dbc?filename=acura_rdx_2020_can_generated.dbc", [], "text/plain", ""}, [], []),
    Body == "Body is empty\n".

post_dbc_wrong_file_test(_Config) ->
    {ok, {{_Version, 400, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(post, {"http://localhost:8080/api/dbc?filename=acura_rdx_2020_can_generated", [], "text/plain", ""}, [], []),
    Body == "Wrong file name format, it should be .dbc file\n".

post_dbc_ok_test(_Config) ->
    PostBody = "BO_ 304 GAS_PEDAL_2: 8 PCM",
    {ok, {{_Version, 202, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(post, {"http://localhost:8080/api/dbc?filename=acura_rdx_2020_can_generated.dbc", [], "text/plain", PostBody}, [], []),
    Body == "The file is in process!\n".

hello_test(_Config) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080", []}, [], []),
    Body == "Hello Erlang!\n".

handle_hash_nok_test(_Config) ->
    {ok, {{_Version, 400, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080/hash", []}, [], []),
    Body == "Wrong method, please use DELETE".

handle_hash_ok_test(_Config) ->
    meck:expect(file, list_dir, fun(_Dirname) -> {ok, []} end),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(delete, {"http://localhost:8080/hash", []}, [], []),
    Body == "Hash is cleared".

handle_hash_file_nok_test(_Config) ->
    {ok, {{_Version, 404, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(delete, {"http://localhost:8080/hash?filename=acura_rdx_2020_can_generated.dbc", []}, [], []),
    Body == "Hash for such file is not exist".
