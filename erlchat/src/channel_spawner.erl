-module(channel_spawner).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/1]).
-export([terminate_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, #{}).

start_child(ChannelUuid) ->
    FoundPid = case channel_tracker:get_pid(ChannelUuid) of
        no_pid ->
            {ok, Pid} = supervisor:start_child(?MODULE, [ChannelUuid]),
            channel_tracker:subscribe(ChannelUuid, Pid),
            Pid;
        P -> P
    end,
    {ok, FoundPid}.

terminate_child(Pid) when is_pid(Pid) ->
    supervisor:terminate_child(?MODULE, Pid);

terminate_child(Uuid) ->
    Pid = channel_tracker:get_pid(Uuid),
    channel_tracker:unsubscribe(Uuid),
    supervisor:terminate_child(?MODULE, Pid).

init(_) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        % This means maximum of 1 restart is allowed within 5 seconds
        % http://erlang.org/doc/design_principles/sup_princ.html#maximum-restart-intensity
        intensity => 1,
        period => 5
    },
    {ok, {SupFlags, [
        #{
            id => channel_spawn_id,
            start => {channel_spawn, start_link, []},
            restart => permanent,
            shutdown => 5000, % 5 seconds
            type => worker
        }
    ]}}.
