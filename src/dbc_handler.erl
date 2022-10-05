-module(dbc_handler).
-behaviour(cowboy_handler).

-include("dbc.hrl").

-export([
	init/2
]).

-export([handle_file/2, make_dbc_root_folder/0]).

make_dbc_root_folder() ->
    case file:make_dir(?DBC_FOLDER) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} -> {error, Reason}
    end.

create_file(File, Data) ->
    ok = file:write_file(?DBC_FOLDER ++ "/" ++ erlang:binary_to_list(File), [Data]).

handle_file(Filename, Data) ->
    create_file(Filename, Data),
    ok = dbc_file_parser:parse(Filename).

start_handle_process(Filename, Data) ->
    _Pid = spawn_link(dbc_handler, handle_file, [Filename, Data]).

init(Req0=#{path := <<"/api/dbc">>, method := <<"POST">>}, State) ->
    QsVals = cowboy_req:parse_qs(Req0),
    {_, Filename} = lists:keyfind(<<"filename">>, 1, QsVals),
    {ok, Data, _} = cowboy_req:read_body(Req0),
    Req = cowboy_req:reply(202,
        #{<<"content-type">> => <<"text/plain">>},
        <<"The file is in process!\n">>,
        Req0),
    start_handle_process(Filename, Data),
    {ok, Req, State};
init(Req0=#{path := <<"/api/dbc">>}, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello Erlang!\n">>,
        Req0),
    {ok, Req, State}.
