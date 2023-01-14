-module(index).

-include("../../include/dbc.hrl").

-export([
    prepare_index/2,
    is_exist/1,
    create_file/1
]).

-spec prepare_index(Directory::string(), BinFileName::binary()) -> 'ok' | {'error', atom()}.
-spec is_exist(File::binary()) -> boolean().
-spec create_file(Directory::string()) -> string().

prepare_index(Directory, BinFileName) ->
    logger:debug("prepare_index for directory ~p File ~p", [Directory, BinFileName]),
    BinaryData = files:read(BinFileName),
    Strings =  [binary_to_list(Data) || Data <- binary:split(BinaryData,<<"\n">>,[global])],
    MessageIds = [Id  || String <- Strings, (Id = get_message_id(String)) /= ok],
    Data = #{msgIds => MessageIds},
    json:write(Data, Directory, ?INDEX_FILE).

get_message_id(String) ->
    StrsList = string:split(String, " ", all),
    messages:get_message_id(StrsList).

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

