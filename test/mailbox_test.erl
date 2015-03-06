-module(mailbox_test).
-include_lib("eunit/include/eunit.hrl").

new_mailbox_test() ->
    Pid = mailbox:new_box(1),
    {BoxId, Messages, _} = mailbox:check_mail(Pid),
    ?assertEqual(1, BoxId),
    ?assertEqual([], Messages),
    mailbox:stop(Pid).

add_message_test() ->
    Pid = mailbox:new_box(1),
    {BoxId, Messages, _} = mailbox:deliver(Pid, "Hello"),
    ?assertEqual(1, BoxId),
    ?assertEqual(["Hello"], Messages),
    mailbox:stop(Pid).

add_listener_test() ->
    Pid = mailbox:new_box(1),
    {_BoxId, Messages, Listeners} = mailbox:add_listener(Pid, self()),
    ?assertEqual([self()], Listeners),
    ?assertEqual([], Messages),
    mailbox:stop(Pid).

notify_listener_test() ->
    Pid = mailbox:new_box(1),
    Listener = spawn(fun() ->
        receive
            Messages when is_list(Messages) ->
                ?assertEqual(["Pow"], Messages)
        end
    end),
    mailbox:add_listener(Pid, Listener),
    mailbox:deliver(Pid, "Pow"),
    mailbox:stop(Pid).
