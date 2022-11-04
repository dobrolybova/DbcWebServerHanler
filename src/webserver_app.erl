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
    ok = logger:set_primary_config(level, notice),
    Config = #{config => #{file => "./info.log"}, level => info},
    ok = logger:add_handler(parser,logger_std_h,Config),
    ok = logger:add_handler(validation,logger_std_h,Config),
    ok = logger:add_handler(hash,logger_std_h,Config),
    ok = logger:add_handler(index,logger_std_h,Config),
    ok = logger:add_handler(message,logger_std_h,Config),
    ok = logger:add_handler(metadata,logger_std_h,Config),
    ok = logger:add_handler(root_handler,logger_std_h,Config),
    ok = logger:add_handler(json,logger_std_h,Config).

initiate() ->
	{ok, FilesList} = file:list_dir(?DBC_FOLDER),
	[parser:start_parser_process(erlang:list_to_binary(File), <<>>) || File <- lists:delete("index", FilesList)],
	ok.