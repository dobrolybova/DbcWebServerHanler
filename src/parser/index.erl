-module(index).

-include("../../include/dbc.hrl").

-export([
    prepare_index/2,
    is_exist/1,
    create_file/1
]).

-define(MESSAGE_ID_PREFIX,  "BO_").

-spec prepare_index(Directory::string(), BinFileName::binary()) -> 'ok' | {'error', atom()}.
-spec is_exist(File::binary()) -> boolean().
-spec create_file(Directory::string()) -> string().

prepare_index(Directory, BinFileName) ->
    logger:debug("prepare_index for directory ~p File ~p", [Directory, BinFileName]),
    FileName = erlang:binary_to_list(BinFileName),
    {ok, BinaryData} = file:read_file(?DBC_FOLDER ++ "/" ++ FileName),
    Strings =  [binary_to_list(Data) || Data <- binary:split(BinaryData,<<"\n">>,[global])],
    MessageIds = [erlang:integer_to_list(Id, 16)  || String <- Strings, (Id = get_message_id(String)) /= ok],
    Data = #{msgIds => MessageIds},
    json:write(Data, Directory, ?INDEX_FILE).

get_message_id(Str) ->
    logger:debug("get_message_id for ~p", [Str]),
    StrsList = string:split(Str, " ", all),
    case lists:nth(1, StrsList) of
        ?MESSAGE_ID_PREFIX ->
            convert_str_id_to_int(lists:nth(2, StrsList));
        _ -> 
            ok
    end.

convert_str_id_to_int(StrId) ->
    logger:debug("convert_str_id_to_int ~p", [StrId]),
    try Id = erlang:list_to_integer(StrId),
        Id
    catch error:_Error -> 
        ok
    end.

is_exist(File) ->
    FileName = erlang:binary_to_list(File),
    {Res, Data} = file:list_dir(?DBC_FOLDER ++ "/index/" ++ FileName),
    logger:debug("is_exist File ~p Res ~p Data ~p", [File, Res, Data]),
    case {Res, Data} of
        {ok,[]} -> false;
        {ok, _} -> true;
        {_, _ } -> false
    end.

create_file(Directory) ->
    logger:debug("create_file Directory ~p", [Directory]),
    File = erlang:list_to_binary(Directory ++ "/" ++ ?INDEX_FILE), 
    files:create(File).   

