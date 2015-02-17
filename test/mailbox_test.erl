-module(mailbox_test).
-include_lib("eunit/include/eunit.hrl").

new_mailbox_test() ->
    Pid = spawn_link(mailbox, start, [1]),
    Pid ! {self(), {mail, check_mailbox}},
    receive
        {Pid, {box_state, BoxId, Messages, _}} ->
            ?assertEqual(1, BoxId),
            ?assertEqual([], Messages)
    end,
    Pid ! exit.


add_message_test() ->
    Pid = spawn_link(mailbox, start, [1]),
    Pid ! {self(), {mail, {add_message, "Hello"}}},
    receive
        {Pid, {box_state, BoxId, Messages, _}} ->
            ?assertEqual(1, BoxId),
            ?assertEqual(["Hello"], Messages)
    end,
    Pid ! exit.

add_listener_test() ->
    Pid = spawn_link(mailbox, start, [1]),
    Pid ! {self(), {mail, {add_listener, self()}}},
    receive
        {Pid, {box_state, _BoxId, Messages, Listeners}} ->
            ?assertEqual([self()], Listeners),
            ?assertEqual([], Messages)
    end,
    Pid ! exit.

notify_listener_test() ->
    Pid = spawn_link(mailbox, start, [1]),
    Pid ! {self(), {mail, {add_listener, self()}}},
    Pid ! {self(), {mail, {add_message, "Hello"}}},
    receive
        Messages when is_list(Messages) ->
            ?assertEqual(["Hello"], Messages)
    end,
    Pid ! exit.
