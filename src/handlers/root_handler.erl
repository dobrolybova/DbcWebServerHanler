-module(root_handler).
-behaviour(cowboy_handler).

-include("../../include/http_status.hrl").

-export([
	init/2
]).

init(Req0=#{path := <<"/">>}, State) ->
    logger:info("Root handle"),
    Req = cowboy_req:reply(?OK,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello Erlang!\n">>,
        Req0),
    {ok, Req, State}.