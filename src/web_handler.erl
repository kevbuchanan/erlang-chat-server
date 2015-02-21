-module(web_handler).

-export([init/2]).
-export([handle_req/3]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    Response = handle_req(Method, Path, Req),
    {ok, Response, Opts}.

handle_req(<<"GET">>, <<"/chat">>, Req) ->
    chat_room:join(1),
    response(<<"Chat!">>, Req);

handle_req(<<"GET">>, <<"/messages">>, Req) ->
    chat_room:connect(1, self()),
    timer:sleep(5),
    chat_room:send_mail(1, "Getting Mail"),
    receive
        Messages -> response(Messages, Req)
    end;

handle_req(<<"POST">>, <<"/message">>, Req) ->
    chat_room:send_mail(1, "My First Message"),
    response(Req);

handle_req(_, _, Req) ->
    response(<<"Not Found">>, Req).

response(Req) ->
    cowboy_req:reply(200, Req).

response(Body, Req) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Body, Req).
