-module(channel_manager).
-behaviour(gen_server).

-export([
    start_link/0,
    subscribe/2,
    unsubscribe/2,
    notify_subscribers/2,
    notify_subscribers/3,
    init/1,
    handle_cast/2,
    handle_call/3
]).

-include_lib("kernel/include/logger.hrl").

% Functions

notify_ws_pids([WsPid | OtherWsPids], Msg) ->
    WsPid ! {refresh, Msg},
    notify_ws_pids(OtherWsPids, Msg);

notify_ws_pids([], _Msg) ->
    done.

% Public

start_link() -> gen_server:start_link(
    {local, ?MODULE}, ?MODULE, no_passed_state, []).

subscribe(ChannelUuid, WebsocketPid) ->
    gen_server:cast(?MODULE, {subscribe, ChannelUuid, WebsocketPid}).

unsubscribe(ChannelUuid, WebsocketPid) ->
    gen_server:cast(?MODULE, {unsubscribe, ChannelUuid, WebsocketPid}).

notify_subscribers(ChannelUuid, Msg) ->
    gen_server:cast(?MODULE, {notify_subscribers, ChannelUuid, Msg}).

notify_subscribers(NotifierWsPid, ChannelUuid, Msg) when is_pid(NotifierWsPid) ->
    gen_server:cast(?MODULE, {notify_subscribers, ChannelUuid, NotifierWsPid, Msg}).


% Private

init(_Arg) ->
    {ok, #{channel_map => #{}}}.

handle_cast({subscribe, ChannelUuid, WebsocketPid}, State) ->
    WsPidList = nested:get([channel_map, ChannelUuid, ws_pid_list], State, []),
    {noreply, nested:put(
        [channel_map, ChannelUuid, ws_pid_list],
        WsPidList ++ [WebsocketPid],
        State)};

handle_cast({unsubscribe, ChannelUuid, WebsocketPid}, State) ->
    WsPidList = nested:get([channel_map, ChannelUuid, ws_pid_list], State, no_list),

    NewState = case WsPidList of
        no_list -> State;
        List -> nested:update(
            [channel_map, ChannelUuid, ws_pid_list],
            lists:delete(WebsocketPid, List),
            State)
    end,

    {noreply, NewState};

handle_cast({notify_subscribers, ChannelUuid, Msg}, State) ->
    WsPidList = nested:get([channel_map, ChannelUuid, ws_pid_list], State, []),
    notify_ws_pids(WsPidList, Msg),
    {noreply, State};

handle_cast({notify_subscribers, ChannelUuid, NotifierWsPid, Msg}, State) when is_pid(NotifierWsPid) ->
    WsPidList = nested:get([channel_map, ChannelUuid, ws_pid_list], State, []),

    notify_ws_pids(
        % Only other subscribers are notified
        % not the one who inititiated it.
        [WsPid || WsPid <- WsPidList, WsPid =/= NotifierWsPid],
        Msg),
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, nothing, State}.
