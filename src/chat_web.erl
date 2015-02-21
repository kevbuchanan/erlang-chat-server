-module(chat_web).

-export([start/0]).

start() ->
    chat_room:start_link(),
    Dispatch = cowboy_router:compile([{'_', [{'_', web_handler, []}]}]),
    cowboy:start_http(http, 100, [{port, 3000}], [{env, [{dispatch, Dispatch}]}]).

stop() ->
    chat_room:stop(),
    ok.
