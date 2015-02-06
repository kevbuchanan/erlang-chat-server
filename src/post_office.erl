-module(post_office).
-behaviour(gen_server).

-export([start_link/0, stop/0, get_mailbox/1, delete_mailbox/1, send_mail/2, status/0]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_mailbox(ListenerId) ->
    gen_server:call(?MODULE, {get_mailbox, ListenerId}).

delete_mailbox(ListenerId) ->
    gen_server:cast(?MODULE, {delete_mailbox, ListenerId}).

send_mail(ListenerId, Message) ->
    gen_server:cast(?MODULE, {send_mail, ListenerId, Message}).

status() ->
    gen_server:call(?MODULE, status).

stop() ->
    gen_server:cast(?MODULE, stop).

get_mailbox(ListenerId, Mailboxes) ->
    lists:filter(fun({Id, _}) -> Id == ListenerId end, Mailboxes).

init(_) ->
    {ok, []}.

handle_call({get_mailbox, ListenerId}, _From, Mailboxes) ->
    case get_mailbox(ListenerId, Mailboxes) of
        [] ->
            Pid = spawn_link(mailbox, start, [ListenerId]),
            Mailbox = {ListenerId, Pid},
            {reply, {ok, Mailbox}, [Mailbox | Mailboxes]};
        [Mailbox] ->
            {reply, {ok, Mailbox}, Mailboxes}
    end;

handle_call(status, _From, State) ->
    {reply, State, State};

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast({send_mail, ListenerId, Message}, Mailboxes) ->
    case get_mailbox(ListenerId, Mailboxes) of
        [{_Id, Pid}] ->
            Pid ! {self(), {mail, add_message, Message}},
            receive
                _ -> ok
            end;
        _ -> ok
    end,
    {noreply, Mailboxes};

handle_cast({delete_mailbox, ListenerId}, Mailboxes) ->
    NewMailboxes = lists:filter(fun({Id, Pid}) ->
        case Id /= ListenerId of
            false -> Pid ! exit, false;
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

terminate(_, _) ->
    ok.
