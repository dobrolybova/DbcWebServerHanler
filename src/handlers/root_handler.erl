-module(root_handler).
-behaviour(cowboy_handler).

-export([
	init/2
]).

init(Req0=#{path := <<"/">>}, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello Erlang!\n">>,
        Req0),
    {ok, Req, State}.