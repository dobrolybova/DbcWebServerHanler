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
    logger:debug("Start handle file Directory ~p Id ~p", [Directory, Id]),
    FileName = create_file(Directory, Id),
    {File, Directory} = parse_filename(FileName),
    MessageIdData = id_map(FileName, Id),
    MessageNameData = name_map(FileName, Id),
    MessageSize = size_map(FileName, Id),
    MessageSender = sender_map(FileName, Id),
    TmpData1 = maps:merge(MessageIdData, MessageNameData),
    TmpData2 = maps:merge(TmpData1, MessageSize),
    Data = maps:merge(TmpData2, MessageSender),
    ok = write(Data, Directory, File).

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

parse_file(FileName, Id) ->
    {File, Directory} = parse_filename(FileName),
    DbcFile = string:prefix(Directory, "dbc/index"),
    logger:debug("Message name map or File ~p DbcFile ~p Directory ~p Id ~p", [File, DbcFile, Directory, Id]),
    BinaryData = files:read(erlang:list_to_binary(DbcFile)),
    [binary_to_list(Data) || Data <- binary:split(BinaryData,<<"\n">>,[global])].

id_map(FileName, Id) ->
    logger:debug("Message id map for File ~p Id ~p", [FileName, Id]),
    #{<<"Id">> => Id}.

name_map(FileName, Id) ->
    Strings =  parse_file(FileName, Id),
    MessageName = [Name  || String <- Strings, (Name = get_message_name(String, Id)) /= ok],
    #{<<"msgName">> => erlang:list_to_binary(lists:nth(1, MessageName))}.

size_map(FileName, Id) ->
    Strings =  parse_file(FileName, Id),
    MessageSize = [Size  || String <- Strings, (Size = get_message_size(String, Id)) /= ok],
    #{<<"msgSize">> => erlang:list_to_binary(lists:nth(1, MessageSize))}.

sender_map(FileName, Id) ->
    Strings =  parse_file(FileName, Id),
    MessageSender = [Size  || String <- Strings, (Size = get_message_sender(String, Id)) /= ok],
    #{<<"msgSender">> => erlang:list_to_binary(lists:nth(1, MessageSender))}.

get_message_id(StrsList) ->
    logger:debug("get_message_id for ~p", [StrsList]),
    case lists:nth(1, StrsList) of
        ?MESSAGE_ID_PREFIX ->
            converter:convert_str_id_to_int(lists:nth(2, StrsList));
        _ -> 
            ok
    end.

get_message_info(Str, Id, PosInStr) ->
    logger:debug("get_message_info for str ~p, Id ~p, pos ~p", [Str, Id, PosInStr]),
    StrsList = string:split(Str, " ", all),
    case get_message_id(StrsList) of
        Id ->
            string:strip(lists:nth(PosInStr, StrsList), right, $:);
        _  -> ok
    end.

get_message_name(Str, Id) ->
    get_message_info(Str, Id, 3).

get_message_size(Str, Id) ->
    get_message_info(Str, Id, 4).

get_message_sender(Str, Id) ->
    get_message_info(Str, Id, 5).

write(Data, Directory, File) ->
    logger:debug("write to msg file Data ~p Directory ~p File ~p", [Data, Directory, File]),
    json:write(Data, Directory, File). 