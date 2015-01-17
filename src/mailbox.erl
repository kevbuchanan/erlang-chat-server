-module(mailbox).

-export([start/1, loop/1]).

-record(box_state, {id, messages = []}).

loop(#box_state{messages = Messages} = State) ->
    receive
        {Sender, {mail, check_mailbox}} ->
            Sender ! {self(), State},
            proc_lib:hibernate(?MODULE, loop, [State]);
        {Sender, {mail, add_message, Message}} ->
            NewState = State#box_state{messages = [Message | Messages]},
            Sender ! {self(), NewState},
            proc_lib:hibernate(?MODULE, loop, [NewState])
    end.

start(Id) ->
    proc_lib:hibernate(?MODULE, loop, [#box_state{id = Id}]).
