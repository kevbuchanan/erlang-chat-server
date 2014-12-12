-module(superchat_test).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = application:start(superchat),
    ?assertNot(undefined == whereis(superchat_sup)).
