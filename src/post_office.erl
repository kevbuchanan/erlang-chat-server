-module(post_office).
-behaviour(gen_server).

-export([start_link/0, stop/0, get_mailbox/1, delete_mailbox/1, send_mail/2, broadcast_mail/1, status/0]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_mailbox(SessionId) ->
    gen_server:call(?MODULE, {get_mailbox, SessionId}).

delete_mailbox(SessionId) ->
    gen_server:cast(?MODULE, {delete_mailbox, SessionId}).

send_mail(SessionId, Message) ->
    gen_server:cast(?MODULE, {send_mail, SessionId, Message}).

broadcast_mail(Message) ->
    gen_server:cast(?MODULE, {broadcast_mail, Message}).

status() ->
    gen_server:call(?MODULE, status).

stop() ->
    gen_server:cast(?MODULE, stop).

get_mailbox(SessionId, Mailboxes) ->
    lists:filter(fun({Id, _}) -> Id == SessionId end, Mailboxes).

init(_) ->
    {ok, []}.

handle_call({get_mailbox, SessionId}, _From, Mailboxes) ->
    case get_mailbox(SessionId, Mailboxes) of
        [] ->
            Pid = mailbox:new_box(SessionId),
            Mailbox = {SessionId, Pid},
            {reply, {ok, Mailbox}, [Mailbox | Mailboxes]};
        [Mailbox] ->
            {reply, {ok, Mailbox}, Mailboxes}
    end;

handle_call(status, _From, State) ->
    {reply, State, State};

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast({send_mail, SessionId, Message}, Mailboxes) ->
    case get_mailbox(SessionId, Mailboxes) of
        [{_Id, Pid}] ->
            mailbox:deliver(Pid, Message),
            ok;
        _ -> ok
    end,
    {noreply, Mailboxes};

handle_cast({broadcast_mail, Message}, Mailboxes) ->
    [mailbox:deliver(Pid, Message) || {_Id, Pid} <- Mailboxes],
    {noreply, Mailboxes};

handle_cast({delete_mailbox, SessionId}, Mailboxes) ->
    NewMailboxes = lists:filter(fun({Id, Pid}) ->
        case Id /= SessionId of
            false -> mailbox:stop(Pid), false;
            _ -> true
        end
    end, Mailboxes),
    {noreply, NewMailboxes};

handle_cast(stop, State) ->
    {stop, normal, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, Mailboxes) ->
    [mailbox:stop(Pid) || {_Id, Pid} <- Mailboxes],
    ok.
