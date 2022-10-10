-module(dbc_handler).
-behaviour(cowboy_handler).

-include("dbc.hrl").

-export([init/2]).

-export([handle_file/2, handle_file_on_init/1, make_dbc_root_folder/0, initiate/0]).

make_dbc_root_folder() ->
    case file:make_dir(?DBC_FOLDER) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} -> {error, Reason}
    end.

initiate() ->
    {ok, FilesList} = file:list_dir(?DBC_FOLDER),
    [handle_files_on_init(File) || File <- lists:delete("index", FilesList)],
    ok.

handle_files_on_init(File) ->
    _Pid = spawn(dbc_handler, handle_file_on_init, [File]).

handle_file_on_init(File) ->
    {ok, Data} = file:read_file(?DBC_FOLDER ++ "/" ++ File),
    handle_file(erlang:list_to_binary(File), Data).

update_hash_wtite_file(FileName, Hash, Data) ->
    persistent_term:put(FileName, Hash),
    ok = file:write_file(?DBC_FOLDER ++ "/" ++ FileName, [Data]),
    ok.


prepare_file(File, Data) ->
    FileName = erlang:binary_to_list(File),
    Hash = erlang:md5(Data),
    try ExistHash = persistent_term:get(FileName),
        if Hash == ExistHash ->
            exist;
        true ->
            update_hash_wtite_file(FileName, Hash, Data)
        end
    catch error:_Error -> 
        update_hash_wtite_file(FileName, Hash, Data)
    end.

handle_file(Filename, Data) ->
    % io:format("Start Sleep ~n"),
    % timer:sleep(20000),
    % io:format("Stop Sleep~n"),
    case prepare_file(Filename, Data) of
        ok -> dbc_file_parser:parse(Filename);
        _ -> ok
    end.

start_handle_process(Filename, Data) ->
    Pid = spawn(dbc_handler, handle_file, [Filename, Data]),
    persistent_term:put(Filename, Pid).

file_handling_status(Filename) ->
    try Pid = persistent_term:get(Filename),
        process_handling_status(Pid)
    catch error: _Error ->
        {400, "The file was not loaded\n"}
    end.

process_handling_status(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> {202, "Still in progress\n"};
        false -> {200, "The file is handled\n"}
    end.

parse_post_request(Req0) ->
    QsVals = cowboy_req:parse_qs(Req0),
    {_, Filename} = lists:keyfind(<<"filename">>, 1, QsVals),
    {ok, Data, _} = cowboy_req:read_body(Req0),
    {Filename, Data}.

prepare_post_response_data(Result, Reason) ->
    case Result of
        true -> {200, "The file is in process!\n"};
        false -> {400, Reason}
    end.

init(Req0=#{path := <<"/api/dbc">>, method := <<"POST">>}, State) ->
    {Filename, Data} = parse_post_request(Req0),
    {Result, Reason} = dbc_request_verification:is_data_ok(Filename, Data),
    {Status, Body} = prepare_post_response_data(Result, Reason),
    start_handle_process(Filename, Data),
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"text/plain">>},
        erlang:list_to_binary(Body),
        Req0),
    {ok, Req, State};
init(Req0=#{path := <<"/api/dbc">>, method := <<"GET">>}, State) ->
    QsVals = cowboy_req:parse_qs(Req0),
    {_, Filename} = lists:keyfind(<<"filename">>, 1, QsVals),
    {Status, Body} = file_handling_status(Filename),
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"text/plain">>},
        erlang:list_to_binary(Body),
        Req0),
    {ok, Req, State}.