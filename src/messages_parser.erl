-module(messages_parser).

-include("dbc.hrl").

-export([prepare_msg/2]).

prepare_msg(Directory, FileName) ->
    Ids = json_wrapper:get_messages_ids(erlang:binary_to_list(FileName)),
    [create_msg_file(Directory, Id) || Id <- Ids],
    ok.

create_msg_file(Directory, Id) ->
    {ok, IoDevice} = file:open(Directory ++ "/msg_" ++ Id, [write]),
    ok = file:close(IoDevice).