-module(log).

-export([
    set_level/1,
    add_handlers/0
]).

-spec set_level(Level::atom()) -> 'ok'.
-spec add_handlers() -> 'ok'.

set_level(Level) ->
    logger:set_primary_config(level, Level).

config() ->
    #{config => #{file => "./info.log"}, level => info}.

add_handlers() ->
    Config = config(),
    Modules = ['dbc_handler',
               'hash_handler',
               'root_handler',
               'index', 
               'message',
               'metadata',
               'parser',
               'files',
               'hash',
               'json',
               'pid',
               'rest',
               'validation',
               'webserver_app',
               'webserver_sup',
               'webserver'
            ],
    [ok = logger:add_handler(Module,logger_std_h,Config) || Module <- Modules],
    ok.
