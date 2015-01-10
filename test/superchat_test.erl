-module(superchat_test).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = superchat_app:start(),
    ?assertNot(undefined == whereis(superchat_sup)).
