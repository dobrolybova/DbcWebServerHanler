-module(hash_handler).

-export([init/2, get_file_hash/1, get_data_hash/1]).

-include("dbc.hrl").

-spec get_data_hash(Data::binary()) -> binary().
-spec get_file_hash(File::string()) -> binary() | atom().

get_data_hash(Data) ->
    erlang:md5(Data).

get_file_hash(File) ->
    try CurrHash = persistent_term:get(File),
        CurrHash
    catch error:Error -> Error        
    end.

clear_all() ->
    {ok, FilesList} = file:list_dir(?DBC_FOLDER),
    [persistent_term:erase(Filename) || Filename <- lists:delete("index", FilesList)],
    {200, "Hash is cleared"}.

clear_for_file(Filename) ->
    Res = persistent_term:erase(Filename),
    case Res of
        true  -> {200, "Hash for file" ++ Filename ++ " is cleared"};
        false -> {400, "Hash for such file is not exist"}
    end.

init(Req0=#{path := <<"/hash">>, method := <<"DELETE">>}, State) ->
    QsVals = cowboy_req:parse_qs(Req0),
    FileSearchRes = lists:keyfind(<<"filename">>, 1, QsVals),
    case FileSearchRes of
        false -> 
            {Res, Body} = clear_all();
        {_, File} ->
            Filename = erlang:binary_to_list(File),
            {Res, Body} = clear_for_file(Filename)
    end,
    Req = cowboy_req:reply(Res,
        #{<<"content-type">> => <<"text/plain">>},
        erlang:list_to_binary(Body),
        Req0),
    {ok, Req, State};
init(Req0=#{path := <<"/hash">>}, State) ->
    Req = cowboy_req:reply(400,
        #{<<"content-type">> => <<"text/plain">>},
        erlang:list_to_binary("Wrong method, please use DELETE"),
        Req0),
    {ok, Req, State}.