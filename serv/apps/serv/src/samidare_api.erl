-module(samidare_api).
-export([init/2]).

init(Req0, State = [other]) ->
    Req = not_found(Req0),
    {ok, Req, State};
init(Req0, State = [db]) ->
    {Val, StCode} = case db_h:select("nothing") of
        {ok, Res} -> {Res, 200};
        {error, Reason} -> {Reason, 400}
    end,
    Res2 = io_lib:format("~p", [Val]),
    Req = cowboy_req:reply(StCode, #{}, Res2, Req0),
    {ok, Req, State};
init(Req0, State = [echo]) ->
    Method = cowboy_req:method(Req0),
    #{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req0),
    Req = echo(Method, Echo, Req0),
    {ok, Req, State}.

not_found(Req) ->
    cowboy_req:reply(400, #{}, <<"not found">> , Req).

echo(<<"GET">>, undefined, Req) ->
    cowboy_req:reply(400, #{}, <<"Missing echo parameter.">>, Req);
echo(<<"GET">>, Echo, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain; charset=utf-8">>
    }, Echo, Req);
echo(_, _, Req) ->
    % Method not allowed.
    cowboy_req:reply(405, Req).
