-module(mailbox).

-export([start/1, loop/1]).

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

start(SessionId) ->
    proc_lib:hibernate(?MODULE, loop, [#box_state{id = SessionId}]).
