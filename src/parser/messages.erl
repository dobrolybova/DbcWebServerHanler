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
    write_message_id(FileName, Id).

create_file(Directory, Id) ->
    logger:debug("create_file for message Directory ~p Id ~p", [Directory, Id]),
    File = erlang:list_to_binary(Directory ++ "/msg_0X" ++ Id), 
    files:create(File).

write_message_id(FileName, Id) ->
    {File, Directory} = parse_filename(FileName),
    logger:debug("Write message id for Directory ~p File ~p Id ~p", [Directory, File, Id]),
    Data = #{<<"Id">> => Id},
    ok = json:write(Data, Directory, File).

parse_filename(FileName) ->
    PathList = string:split(FileName, "/", all),
    File = lists:last(PathList),
    DirList = lists:join("/", lists:delete(File, PathList)),
    Directory = lists:concat(DirList),
    {File, Directory}.
