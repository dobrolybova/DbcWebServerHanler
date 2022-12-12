-module(rest).

-include("../../include/dbc.hrl").
-include("../../include/http_status.hrl").

-export([
    get_filename/1,
    get_body/1,
    dbc_post_response/2,
    dbc_get_response/1,
    clear_all/0,
    clear/2
]).

-spec get_filename(Req::map()) ->  'false' | binary().
-spec get_body(Req::map()) ->  binary().
-spec dbc_post_response(Filename::binary(), Data::binary()) -> {integer(), string()}.
-spec dbc_get_response(Filename::binary()) -> {integer(), string()}.
-spec clear_all() -> {integer(), string()}.
-spec clear(Filename::binary(), Res::boolean()) -> {integer(), string()}.

get_filename(Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    try {_, Filename} = lists:keyfind(<<"filename">>, 1, QsVals),
        Filename
    catch error:_Error -> false
    end.

get_body(Req) -> 
    {ok, Data, _} = cowboy_req:read_body(Req),
    Data.

dbc_post_response(Filename, Data) ->
    {Result, Reason} = validation:is_data_ok(Filename, Data),
    case Result of
        true -> {?ACCEPTED, "The file is in process!\n"};
        false -> {?BAD_REQUEST, Reason}
    end.

dbc_get_response(Filename) ->
    logger:debug("Try to get Pid for file ~p", [Filename]),
    try Pid = pid:get(Filename),
        get_process_result(Pid, Filename)
    catch error: _Error ->
        {?NOT_FOUND, "The file was not loaded\n"}
    end.

get_process_result(Pid, Filename) ->
    case erlang:is_process_alive(Pid) of
        true ->  {?ACCEPTED, "Still in progress\n"};
        false -> get_meta_result(Filename)
    end.

get_meta_result(Filename) ->
    case metadata:get_status(Filename) of
        ?DONE -> {?OK, "The file is handled\n"};
        _     -> {?INTERNAL_SERVER_ERROR, "Internal error\n"}
    end.

clear_all() ->
    {?OK, "Hash is cleared"}.

clear(Filename, Res) ->
    File = erlang:binary_to_list(Filename),
    case Res of
        true  -> {?OK, "Hash for file" ++ File ++ " is cleared"};
        false -> {?NOT_FOUND, "Hash for such file is not exist"}
    end.
    