-module(superchat_app).

-export([start/0, start/2, stop/1]).

start() ->
    application:ensure_all_started(superchat),
    ok.

start(_StartType, _StartArgs) ->
    erlang:display("Starting"),
    superchat_sup:start_link().

stop(_State) -> ok.
