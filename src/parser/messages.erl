-module(messages).

-include("../../include/dbc.hrl").

-export([
    prepare_msg/2,
    get_message_id/1
]).

-spec prepare_msg(Directory::string(), FileName::binary()) -> 'ok'.
-spec get_message_id(StrsList::[string()]) -> 'ok' | integer().

prepare_msg(Directory, FileName) ->
    logger:debug("prepare_msg Directory ~p FileName ~p", [Directory, FileName]),
    Ids = json:get_messages_ids(erlang:binary_to_list(FileName)),
    [handle_file(Directory, Id) || Id <- Ids],
    ok.

handle_file(Directory, Id) ->
    FileName = create_file(Directory, Id),
    {File, Directory} = parse_filename(FileName),
    MessageIdData = id_map(FileName, Id),
    MessageNameData = name_map(FileName, Id),
    Data = maps:merge(MessageIdData, MessageNameData),
    ok = write(Data, Directory, File),
    ok = size_map(FileName),
    ok = sender_map(FileName).

create_file(Directory, Id) ->
    logger:debug("create_file for message Directory ~p Id ~p", [Directory, Id]),
    HexId = erlang:integer_to_list(Id, 16),
    File = erlang:list_to_binary(Directory ++ "/msg_0X" ++ HexId), 
    files:create(File).

parse_filename(FileName) ->
    logger:debug("Parse filename ~p ", [FileName]),
    PathList = string:split(FileName, "/", all),
    File = lists:last(PathList),
    DirList = lists:join("/", lists:delete(File, PathList)),
    Directory = lists:concat(DirList),
    {File, Directory}.

id_map(FileName, Id) ->
    {File, Directory} = parse_filename(FileName),
    logger:debug("Message id map for Directory ~p File ~p Id ~p", [Directory, File, Id]),
    #{<<"Id">> => Id}.

name_map(FileName, Id) ->
    {File, Directory} = parse_filename(FileName),
    DbcFile = string:prefix(Directory, "dbc/index"),
    logger:debug("Message name map or File ~p DbcFile ~p Directory ~p Id ~p", [File, DbcFile, Directory, Id]),
    BinaryData = files:read(erlang:list_to_binary(DbcFile)),
    Strings =  [binary_to_list(Data) || Data <- binary:split(BinaryData,<<"\n">>,[global])],
    MessageNames = [Name  || String <- Strings, (Name = get_message_name(String, Id)) /= ok],
    MessageName = case MessageNames of
                      [] -> "";
                      _ -> erlang:list_to_binary(lists:nth(1, MessageNames))
                  end,
    #{<<"msgName">> => MessageName}.

size_map(_File) ->
    ok.

sender_map(_File) ->
    ok.

get_message_id(StrsList) ->
    logger:debug("get_message_id for ~p", [StrsList]),
    case lists:nth(1, StrsList) of
        ?MESSAGE_ID_PREFIX ->
            converter:convert_str_id_to_int(lists:nth(2, StrsList));
        _ -> 
            ok
    end.

get_message_name(Str, Id) ->
    logger:debug("get_message_name for ~p", [Str]),
    StrsList = string:split(Str, " ", all),
    case get_message_id(StrsList) of
        Id ->
            string:strip(lists:nth(3, StrsList), right, $:);
        _  -> ok
    end.        

write(Data, Directory, File) ->
    logger:debug("write to msg file Data ~p Directory ~p File ~p", [Data, Directory, File]),
    json:write(Data, Directory, File). 