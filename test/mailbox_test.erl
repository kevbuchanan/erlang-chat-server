-module(mailbox_test).
-include_lib("eunit/include/eunit.hrl").

new_mailbox_test() ->
    Pid = spawn_link(mailbox, start, [1]),
    Pid ! {self(), {mail, check_mailbox}},
    receive
        {Pid, {box_state, BoxId, Messages}} ->
            ?assertEqual(1, BoxId),
            ?assertEqual([], Messages)
    end.

add_message_test() ->
    Pid = spawn_link(mailbox, start, [2]),
    Pid ! {self(), {mail, add_message, "Hello"}},
    receive
        {Pid, {box_state, BoxId, Messages}} ->
            ?assertEqual(2, BoxId),
            ?assertEqual(["Hello"], Messages)
    end.
