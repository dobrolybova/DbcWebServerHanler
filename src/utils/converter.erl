-module(converter).

-include("../../include/dbc.hrl").

-export([
    convert_str_id_to_int/1
]).

-spec convert_str_id_to_int(StrId::string()) -> 'ok' | integer().

convert_str_id_to_int(StrId) ->
    logger:debug("convert_str_id_to_int ~p", [StrId]),
    try Id = erlang:list_to_integer(StrId),
        Id
    catch error:_Error -> 
        ok
    end.