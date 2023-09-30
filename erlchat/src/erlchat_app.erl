-module(erlchat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/:subscriber_uuid", ws_handler, #{}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        server_listener,
        [{port, env:get("WS_PORT")}],
        #{env => #{dispatch => Dispatch}}),
	erlchat_sup:start_link().

stop(_State) ->
	ok.
