-module(webserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = dbc_handler:make_dbc_root_folder(),
	ok = dbc_handler:initiate(),
	Dispatch = cowboy_router:compile([
		{'_', [
            {"/api/dbc", dbc_handler, []},
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