-module(erlchat_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
        #{
            id => user_pool_id,
            start => {user_pool, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => dynamic
        },
        #{
            id => channel_pool_id,
            start => {channel_pool, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => dynamic
        }
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
