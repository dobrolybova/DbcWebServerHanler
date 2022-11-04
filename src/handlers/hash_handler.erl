-module(hash_handler).

-export([
    init/2
]).

init(Req0=#{path := <<"/hash">>, method := <<"DELETE">>}, State) ->
    FileName = rest:get_filename(Req0),
    % {Res, Body} = rest:prepare_hash_delete_response(FileName),
    {Result, Body} =  case FileName of
                        false -> 
                            hash:clear_all(),
                            rest:clear_all();
                        File  -> 
                            Res = hash:clear(File),
                            rest:clear(File, Res)
                    end,
    Req = cowboy_req:reply(Result,
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