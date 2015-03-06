-module(mailbox).

-export([new_box/1, start/1, stop/1, deliver/2, check_mail/1, add_listener/2, loop/1]).

-record(box_state, {id, messages = [], listeners = []}).

loop(#box_state{messages = Messages, listeners = Listeners} = State) ->
    receive
        {Sender, {mail, check_mailbox}} ->
            Sender ! {self(), State},
            proc_lib:hibernate(?MODULE, loop, [State]);

        {Sender, {mail, {message, Message}}} ->
            NewState = State#box_state{messages = [Message | Messages]},
            Sender ! {self(), NewState},
            lists:map(fun(Pid) -> Pid ! [Message] end, Listeners),
            proc_lib:hibernate(?MODULE, loop, [NewState]);

        {Sender, {mail, {add_listener, Listener}}} ->
            NewState = State#box_state{listeners = [Listener | Listeners]},
            Sender ! {self(), NewState},
            proc_lib:hibernate(?MODULE, loop, [NewState]);

        exit -> ok;
        _ -> proc_lib:hibernate(?MODULE, loop, [State])
    end.

wait() ->
    receive
        {_Pid, {box_state, BoxId, Messages, Listeners}} ->
            {BoxId, Messages, Listeners}
    end.

deliver(Pid, Message) ->
    Pid ! {self(), {mail, {message, Message}}},
    wait().

check_mail(Pid) ->
    Pid ! {self(), {mail, check_mailbox}},
    wait().

add_listener(Pid, Listener) ->
    Pid ! {self(), {mail, {add_listener, Listener}}},
    wait().

stop(Pid) ->
    Pid ! exit.

new_box(SessionId) ->
    spawn_link(mailbox, start, [SessionId]).

start(SessionId) ->
    proc_lib:hibernate(?MODULE, loop, [#box_state{id = SessionId}]).
