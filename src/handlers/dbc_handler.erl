-module(dbc_handler).
-behaviour(cowboy_handler).

-include("../../include/dbc.hrl").

-export([
    init/2
]).

init(Req0=#{path := <<"/api/dbc">>, method := <<"POST">>}, State) ->
    logger:notice("Handle post"),
    Filename = rest:get_filename(Req0),
    Data = rest:get_body(Req0),
    {Status, Body} = rest:dbc_post_response(Filename, Data),
    parser:start_parser_process(Filename, Data),
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"text/plain">>},
        erlang:list_to_binary(Body),
        Req0),
    {ok, Req, State};
init(Req0=#{path := <<"/api/dbc">>, method := <<"GET">>}, State) ->
    logger:notice("Handle get"),
    Filename = rest:get_filename(Req0),
    {Status, Body} = rest:dbc_get_response(Filename),
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"text/plain">>},
        erlang:list_to_binary(Body),
        Req0),
    {ok, Req, State}.