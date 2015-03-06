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
    Ok = post_office:send_mail(Id, "Message"),
    ?assertEqual(ok, Ok),
    timer:sleep(1),
    {_, Messages, _} = mailbox:check_mail(Pid),
    ?assertEqual(["Message"], Messages),
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
    {_, {_, Pid}} = post_office:get_mailbox(1),
    {_, {_, Pid_2}} = post_office:get_mailbox(2),
    post_office:broadcast_mail({user_left, 1}),
    timer:sleep(1),
    {_, Messages_1, _} = mailbox:check_mail(Pid),
    ?assertEqual([{user_left, 1}], Messages_1),
    {_, Messages_2, _} = mailbox:check_mail(Pid_2),
    ?assertEqual([{user_left, 1}], Messages_2),
    post_office:stop().
