-module(post_office_test).
-include_lib("eunit/include/eunit.hrl").

post_office_init_test() ->
    post_office:start_link(),
    Mailboxes = post_office:status(),
    ?assertEqual([], Mailboxes),
    post_office:stop().

post_office_create_mailbox_test() ->
    post_office:start_link(),
    {_Ok, {Id, _Pid}} = post_office:get_mailbox(3),
    ?assertEqual(Id, 3),
    Mailboxes = post_office:status(),
    ?assertEqual(1, length(Mailboxes)),
    post_office:stop().

post_office_find_mailbox_test() ->
    post_office:start_link(),
    {_Ok, {Id, Pid}} = post_office:get_mailbox(1),
    {_Ok, {Id, Pid_2}} = post_office:get_mailbox(1),
    ?assertEqual(Pid, Pid_2),
    post_office:stop().

post_office_send_mail_test() ->
    post_office:start_link(),
    {_Ok, {Id, Pid}} = post_office:get_mailbox(12),
    Ok = post_office:send_mail(Id, {add_message, "Message"}),
    Pid ! {self(), {mail, check_mailbox}},
    ?assertEqual(ok, Ok),
    receive
        {Pid, {box_state, _, Messages, _}} ->
            ?assertEqual(["Message"], Messages)
    end,
    post_office:stop().

post_office_delete_mailbox_test() ->
    post_office:start_link(),
    {_Ok, {_Id, _Pid}} = post_office:get_mailbox(1),
    post_office:delete_mailbox(1),
    Mailboxes = post_office:status(),
    ?assertEqual([], Mailboxes),
    post_office:stop().

post_office_broadcast_mail_test() ->
    post_office:start_link(),
    post_office:get_mailbox(1),
    post_office:get_mailbox(2),
    Ok = post_office:broadcast_mail({user_left, 1}),
    ?assertEqual(ok, Ok),
    post_office:stop().

