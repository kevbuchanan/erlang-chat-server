-module(chat_room).
-behaviour(gen_server).

-export([start_link/0, status/0, stop/0, join/1, connect/2, send_mail/2]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2, terminate/2]).

start_link() ->
    post_office:start_link(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

status() ->
    gen_server:call(?MODULE, status).

join(SessionId) ->
    gen_server:call(?MODULE, {join, SessionId}).

connect(SessionId, Pid) ->
    gen_server:cast(?MODULE, {connect, SessionId, Pid}).

send_mail(SessionId, Message) ->
    gen_server:cast(?MODULE, {send_mail, SessionId, Message}).

stop() ->
    gen_server:cast(?MODULE, stop).

init(_) ->
    {ok, []}.

handle_call({join, SessionId}, _From, Sessions) ->
    case post_office:get_mailbox(SessionId) of
        {ok, _} ->
            NewSessions = [SessionId | Sessions],
            {reply, NewSessions, NewSessions};
        _ ->
            {reply, error, Sessions}
    end;

handle_call(status, _From, State) ->
    {reply, State, State}.

handle_cast({connect, SessionId, Pid}, Sessions) ->
    post_office:send_mail(SessionId, {add_listener, Pid}),
    {noreply, Sessions};

handle_cast({send_mail, _SessionId, Message}, Sessions) ->
    post_office:broadcast_mail({message, Message}),
    {noreply, Sessions};

handle_cast(stop, State) ->
    {stop, normal, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.
