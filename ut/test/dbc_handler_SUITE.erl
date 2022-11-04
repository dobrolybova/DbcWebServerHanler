-module(dbc_handler_SUITE).

-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         get_dbc_not_loaded_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        get_dbc_not_loaded_test
    ].

init_per_suite(Config) ->
    {ok, App_Start_List} = start([webserver]),
    inets:start(),
    [{app_start_list, App_Start_List}|Config].

init_per_testcase(_, Config) ->
    meck:new(file, [unstick, passthrough]),
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
    {ok, {{_Version, 400, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080/api/dbc?filename=acura_rdx_2020_can_generated.dbc", []}, [], []),
    Body = "The file was not loaded\n".
