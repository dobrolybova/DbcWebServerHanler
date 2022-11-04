-module(messages).

-include("../../include/dbc.hrl").

-export([
    prepare_msg/2
]).

-spec prepare_msg(Directory::string(), FileName::binary()) -> 'ok'.

prepare_msg(Directory, FileName) ->
    logger:notice("prepare_msg ~p", [FileName]),
    Ids = json:get_messages_ids(erlang:binary_to_list(FileName)),
    [create_file(Directory, Id) || Id <- Ids],
    ok.

create_file(Directory, Id) ->
    logger:notice("create_file for message Directore ~p Id ~p", [Directory, Id]),
    File = erlang:list_to_binary(Directory ++ "/msg_0X" ++ Id), 
    files:create(File).
