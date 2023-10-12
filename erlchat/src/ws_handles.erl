-module(ws_handles).
-export([
    on/3
]).

on(WsPid, [<<"subscribe">>], State) ->
    UserUuid = time(),
    {ok, UserPid} = user_spawner:start_child(#{
        uuid => time(), % TODO!
        ws_pids => [WsPid] % TODO need to append WsPid
    }),
    {[], State#{
        user => #{
            uuid => UserUuid,
            pid => UserPid
        }
    }};

on(_WsPid, [<<"chat">>, ChannelUuid, <<"join">>], State = #{ user := #{ pid := UserPid }}) ->
    {ok, ChannelPid} = channel_spawner:start_child(ChannelUuid),
    channel_spawn:join(ChannelPid, UserPid),
    {[], State};

on(_WsPid, [<<"chat">>, ChannelUuid, <<"message">>, Message], State) ->
    ChannelPid = channel_tracker:get_pid(ChannelUuid),
    channel_spawn:notify_members(ChannelPid, Message),
    {[], State}. % Respond nothing, rely on notify_subscribers to do the job
