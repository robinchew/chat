-module(channel_pool).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, #{}).

init(_) ->
    SupFlags = #{
        strategy => one_for_all,
        % This means maximum of 1 restart is allowed within 5 seconds
        % http://erlang.org/doc/design_principles/sup_princ.html#maximum-restart-intensity
        intensity => 1,
        period => 5
    },
    {ok, {SupFlags, [
        #{
            id => channel_tracker_id,
            start => {channel_tracker, start_link, []},
            restart => permanent,
            shutdown => 5000, % 5 seconds
            type => worker,
            modules => dynamic
        },
        #{
            id => channel_spawner_id,
            start => {channel_spawner, start_link, []},
            restart => permanent,
            shutdown => 5000, % 5 seconds
            type => supervisor,
            modules => dynamic
        }
%        },
%        #{
%            id => channel_spawner_children_id,
%            start => {channel_spawner_children, start_children, []},
%            restart => transient,
%            shutdown => 5000, % 5 seconds
%            type => supervisor,
%            modules => dynamic
%        }
    ]}}.
