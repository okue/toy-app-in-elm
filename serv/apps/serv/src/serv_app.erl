-module(serv_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_',
            [ {"/echo",         samidare_api,  [echo]}
            , {"/db",           samidare_api,  [db]}
            , {"/",             cowboy_static, {file, ?ROOT ++ "/index.html"}}
            , {"/index.html",   cowboy_static, {file, ?ROOT ++ "/index.html"}}
            , {"/assets/[...]", cowboy_static, {dir, ?ROOT ++ "/assets"}}
            , {"/output/[...]", cowboy_static, {dir, ?ROOT ++ "/output"}}
            , {'_', samidare_api, [other]}
            ]
        }
    ]),

    {ok, _} = cowboy:start_clear(serv_http,
        [{port, 8000}],
        #{env => #{dispatch => Dispatch}}
    ),

    serv_sup:start_link().

stop(_State) ->
    ok.
