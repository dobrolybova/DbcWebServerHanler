-module(dbc_request_verification).

-export([is_data_ok/2]).

is_data_ok(FileName, Body) ->
    {FileRes, FileReason} = is_file_name_ok(FileName),
    {BodyRes, BodyReason} = is_body_ok(Body),
    case {FileRes, FileReason} of
        {false, _} -> {FileRes, FileReason};
        {true, _}  ->
            case {BodyRes, BodyReason} of
                {false, _} -> {BodyRes, BodyReason};
                {true, _}  -> {true, ""}
            end
    end.


is_body_ok(Body) ->
    if Body == <<>> ->
        {false, "Body is empty"};
    true -> {true, ""}
    end.

is_file_name_ok(FileName) ->
    if erlang:is_binary(FileName) ->
        File = erlang:binary_to_list(FileName);
    true ->
        File = FileName
    end,
    try [_H, _T] = string:split(File, "."),
        {true, ""}
    catch error: _Error ->
        {false, "Wrong file name format, it should be .dbc file"}
    end.