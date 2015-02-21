-module(chat_room_test).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    chat_room:start_link(),
    Clients = chat_room:status(),
    ?assertEqual([], Clients),
    chat_room:stop().

join_creates_new_mailbox_test() ->
    chat_room:start_link(),
    ClientSessions = chat_room:join(5),
    ?assertEqual([5], ClientSessions),
    [{Id, BoxPid}] = post_office:status(),
    ?assertEqual(5, Id),
    BoxPid ! exit,
    chat_room:stop().

flush() ->
    receive
        _ -> flush()
    after
        0 -> true
    end.

connect_adds_connection_pid_test() ->
    chat_room:start_link(),
    chat_room:join(2),
    chat_room:connect(2, self()),
    flush(),
    timer:sleep(1),
    chat_room:send_mail(2, "Hellooo"),
    receive
        Message ->
            ?assertEqual(["Hellooo"], Message)
    end,
    chat_room:join(3),
    chat_room:send_mail(3, "Tacos"),
    receive
        Message2 ->
            ?assertEqual(["Tacos"], Message2)
    end,
    OtherClient = spawn(fun() ->
        receive
            Message3 ->
                ?assertEqual(["Boom"], Message3)
        end
    end),
    chat_room:join(4),
    chat_room:connect(4, OtherClient),
    timer:sleep(1),
    chat_room:send_mail(3, "Boom"),
    chat_room:stop().
