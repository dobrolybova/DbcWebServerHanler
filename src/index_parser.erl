-module(index_parser).

-include("dbc.hrl").

-export([prepare_index/2]).

-define(MESSAGE_ID_PREFIX,  "BO_").

-spec prepare_index(Directory::string(), BinFileName::binary()) -> 'ok' | {'error', atom()}.

prepare_index(Directory, BinFileName) ->
    FileName = erlang:binary_to_list(BinFileName),
    {ok, BinaryData} = file:read_file(?DBC_FOLDER ++ "/" ++ FileName),
    Strings =  [binary_to_list(Data) || Data <- binary:split(BinaryData,<<"\n">>,[global])],
    MessageIds = [erlang:integer_to_list(Id, 16)  || String <- Strings, (Id = get_message_id(String)) /= ok],
    Data = #{msgIds => MessageIds},
    json_wrapper:write(Data, Directory, ?INDEX_FILE).

get_message_id(Str) ->
    StrsList = string:split(Str, " ", all),
    case lists:nth(1, StrsList) of
        ?MESSAGE_ID_PREFIX ->
            convert_str_id_to_int(lists:nth(2, StrsList));
        _ -> 
            ok
    end.

convert_str_id_to_int(StrId) ->
    try Id = erlang:list_to_integer(StrId),
        Id
    catch error:_Error -> 
        ok
    end.

