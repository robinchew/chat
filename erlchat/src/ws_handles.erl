-module(ws_handles).
-export([
    on/2
]).

on(WsPid, [<<"chat">>, ChannelUuid, <<"join">>]) ->
    channel_tracker:subscribe(ChannelUuid, WsPid),
    [];

on(_WsPid, [<<"chat">>, ChannelUuid, <<"message">>, Message]) ->
    channel_tracker:notify_subscribers(ChannelUuid, Message),
    []. % Respond nothing, rely on notify_subscribers to do the job
