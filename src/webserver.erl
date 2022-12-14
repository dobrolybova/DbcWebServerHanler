-module(webserver).

-export([
	start/0,
	stop/0
]).

-define(APPS, [crypto, asn1, public_key, ssl, ranch, cowlib, cowboy, webserver]).

start() ->
	ok = ensure_started(?APPS),
	ok = sync:go().

stop() ->
	ok = sync:stop(),
	ok = stop_apps(lists:reverse(?APPS)).

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps)
	end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
	ok = application:stop(App),
	stop_apps(Apps).