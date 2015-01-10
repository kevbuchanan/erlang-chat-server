-module(chat_web).

-export([start/0]).

start() ->
    Dispatch = cowboy_router:compile([{'_', [{"/", web_handler, []}]}]),
    cowboy:start_http(http, 100, [{port, 3000}], [{env, [{dispatch, Dispatch}]}]).

stop() ->
    ok.
