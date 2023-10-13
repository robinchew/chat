-module(ws_handles).
-export([
    on/3
]).
on(_WsPid, [<<"ping">>], State) ->
    {[], State};

on(WsPid, [<<"subscribe">>], State) ->
    UserUuid = time(),
    {ok, UserPid} = user_spawner:start_child(#{
            uuid => time(), % TODO!
            ws_pids => []
        }),
    user_spawn:register_ws_pid(UserPid, WsPid),
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

on(_WsPid, [<<"chat">>, ChannelUuid, <<"message">>, Message], State = #{ user := #{ uuid := UserUuid }}) ->
    ChannelPid = channel_tracker:get_pid(ChannelUuid),
    channel_spawn:post(ChannelPid, UserUuid, Message),
    {[], State}. % Respond nothing, rely on notify_subscribers to do the job
