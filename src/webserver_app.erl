-module(webserver_app).

-behaviour(application).

-include("../include/dbc.hrl").

-export([
	start/2,
	stop/1
]).

start(_StartType, _StartArgs) ->
	ok = start_logger(),
    ok = files:make_folder(erlang:list_to_binary(?DBC_FOLDER)),
	ok = initiate(),
	Dispatch = cowboy_router:compile([
		{'_', [
            {"/api/dbc", dbc_handler, []},
			{"/hash", hash_handler, []},
			{"/", root_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http_listener,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
	webserver_sup:start_link().

stop(_State) ->
    ok.

start_logger() ->
    ok = log:set_level(notice),
    log:add_handlers().

initiate() ->
	{ok, FilesList} = file:list_dir(?DBC_FOLDER),
	[parser:start_parser_process(erlang:list_to_binary(File), <<>>) || File <- lists:delete("index", FilesList)],
	ok.