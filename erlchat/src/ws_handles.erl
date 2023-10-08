-module(ws_handles).
-export([
    on/2
]).

on(WsPid, [<<"chat">>, ChannelUuid, <<"join">>]) ->
    channel_manager:subscribe(ChannelUuid, WsPid),
    [];

on(WsPid, [<<"chat">>, ChannelUuid, <<"leave">>]) ->
    channel_manager:unsubscribe(ChannelUuid, WsPid),
    [];

on(_WsPid, [<<"ping">>]) ->
    % log then return pong
    % io:format("ping from ~p~n", [WsPid]),
    [<<"pong">>];

on(_WsPid, [<<"chat">>, ChannelUuid, <<"message">>, Message]) ->
    io:format("message ~p~n", [Message]),
    % set the message to TEXT of "chat|<message>" so clients can distinguish
    channel_manager:notify_subscribers(ChannelUuid, [<<"chat|">>, Message]),
    []. % Respond nothing, rely on notify_subscribers to do the job
