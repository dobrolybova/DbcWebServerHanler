-module(messages).

-include("../../include/dbc.hrl").

-export([
    prepare_msg/2
]).

-spec prepare_msg(Directory::string(), FileName::binary()) -> 'ok'.

prepare_msg(Directory, FileName) ->
    logger:debug("prepare_msg Directory ~p FileName ~p", [Directory, FileName]),
    Ids = json:get_messages_ids(erlang:binary_to_list(FileName)),
    [handle_file(Directory, Id) || Id <- Ids],
    ok.

handle_file(Directory, Id) ->
    FileName = create_file(Directory, Id),
    MessageIdData = get_message_id(FileName, Id),
    MessageNameData = write_message_name(FileName, Id),
    {File, Directory} = parse_filename(FileName),
    Data = maps:merge(MessageIdData, MessageNameData),
    write(Data, Directory, File),
    ok = write_size(FileName),
    ok = write_sender(FileName).

create_file(Directory, Id) ->
    logger:debug("create_file for message Directory ~p Id ~p", [Directory, Id]),
    HexId = erlang:integer_to_list(Id, 16),
    File = erlang:list_to_binary(Directory ++ "/msg_0X" ++ HexId), 
    files:create(File).

get_message_id(FileName, Id) ->
    {File, Directory} = parse_filename(FileName),
    logger:debug("Write message id for Directory ~p File ~p Id ~p", [Directory, File, Id]),
    #{<<"Id">> => Id}.

parse_filename(FileName) ->
    logger:debug("Parse filename ~p ", [FileName]),
    PathList = string:split(FileName, "/", all),
    File = lists:last(PathList),
    DirList = lists:join("/", lists:delete(File, PathList)),
    Directory = lists:concat(DirList),
    {File, Directory}.

write_message_name(FileName, Id) ->
    {File, Directory} = parse_filename(FileName),
    DbcFile = string:prefix(Directory, "dbc/index"),
    logger:debug("Write message name for File ~p DbcFile ~p Directory ~p Id ~p", [File, DbcFile, Directory, Id]),
    BinaryData = files:read(erlang:list_to_binary(DbcFile)),
    Strings =  [binary_to_list(Data) || Data <- binary:split(BinaryData,<<"\n">>,[global])],
    MessageNames = [Name  || String <- Strings, (Name = get_message_name(String, Id)) /= ok],
    MessageName = erlang:list_to_binary(lists:nth(1, MessageNames)),
    #{<<"msgName">> => MessageName}.

write_size(_File) ->
    ok.

write_sender(_File) ->
    ok.

get_message_name(Str, Id) ->
    logger:debug("get_message_name for ~p", [Str]),
    StrsList = string:split(Str, " ", all),
    case lists:nth(1, StrsList) of
        ?MESSAGE_ID_PREFIX ->
            case convert_str_id_to_int(lists:nth(2, StrsList)) of
                Id ->
                    string:strip(lists:nth(3, StrsList), right, $:);
                _  -> ok
            end;
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

write(Data, Directory, File) ->
    logger:debug("write to msg file Data ~p Directory ~p File ~p", [Data, Directory, File]),
    json:write(Data, Directory, File). 