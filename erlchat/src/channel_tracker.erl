-module(channel_tracker).
-behaviour(gen_server).

-export([
    start_link/0,
    get_pid/1,
    subscribe/2,
    unsubscribe/1,
    init/1,
    handle_cast/2,
    handle_call/3
]).

-include_lib("kernel/include/logger.hrl").

% Functions

% Public

start_link() -> gen_server:start_link(
    {local, ?MODULE}, ?MODULE, no_passed_state, []).

get_pid(ChannelUuid) ->
    gen_server:call(?MODULE, {get_pid, ChannelUuid}).

subscribe(ChannelUuid, Pid) ->
    gen_server:cast(?MODULE, {subscribe, ChannelUuid, Pid}).

unsubscribe(ChannelUuid) ->
    gen_server:cast(?MODULE, {unsubscribe, ChannelUuid}).


% Private

init(_Arg) ->
    {ok, #{channel_map => #{}}}.

handle_cast({subscribe, ChannelUuid, Pid}, State) ->
    {noreply, nested:put(
        [channel_map, ChannelUuid],
        Pid,
        State)};

handle_cast({unsubscribe, ChannelUuid}, State) ->
    {noreply, nested:remove(
        [channel_map, ChannelUuid],
        State)}.

handle_call({get_pid, ChannelUuid}, _From, State) ->
    {reply, nested:get([channel_map, ChannelUuid], State, no_pid), State};

handle_call(_, _From, State) ->
    {reply, nothing, State}.
