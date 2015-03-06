-module(chat_web).

-export([start/0, stop/0]).

start() ->
    chat_room:start_link(),
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/", cowboy_static, {priv_file, superchat, "index.html"}},
               {'_', web_handler, []}
              ]}
    ]),
    cowboy:start_http(http, 100, [{port, 3000}], [{env, [{dispatch, Dispatch}]}]).

stop() ->
    chat_room:stop(),
    ok.
