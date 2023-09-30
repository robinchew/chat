-module(ws_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req = #{bindings := #{subscriber_uuid := SubscriberUuid}}, State) ->
    % TODO rename subscriber_uuid session_uuid
	{cowboy_websocket, Req, State#{
        subscriber_uuid => SubscriberUuid
    }};

init(Req, State) ->
	{cowboy_websocket, Req, State}.

websocket_init(State) ->
    Pid = self(),
	{[], State#{
        websocket_pid => Pid
    }}.

websocket_handle({text, Data}, State = #{websocket_pid := WsPid}) ->
    Responses = ws_handles:on(WsPid, string:split(Data, "|", all)),
	{[{text, Text} || Text <- Responses], State};

websocket_handle({binary, Data}, State) ->
    io:format("bni~p~n", [Data]),
	{[{binary, Data}], State};

websocket_handle(_Frame, State) ->
	{[], State}.

websocket_info({refresh, Text}, State) ->
    % Received from notify_subscribers
	{[{text, Text}], State};

websocket_info(Info, State) ->
    io:format("iNfo ~p~n", [Info]),
	{[], State}.
