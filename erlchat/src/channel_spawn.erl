-module(channel_spawn).
-behaviour(gen_server).
-export([
    start_link/1,
    join/2,
    post/3
]).
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2
]).

-include_lib("kernel/include/logger.hrl").

% Functions

notify_user_pids([UserPid | OtherUserPids], Msg) ->
    UserPid ! {new_message, Msg},
    notify_user_pids(OtherUserPids, Msg);

notify_user_pids([], _Msg) ->
    done.

% Public API

start_link(ChannelUuid) ->
    %gen_server:start_link({local, Id}, ?MODULE, User, []).
    gen_server:start_link(?MODULE, ChannelUuid, []).

join(ChannelPid, UserPid) ->
    gen_server:cast(ChannelPid, {join, UserPid}).

post(ChannelPid, UserUuid, Msg) ->
    gen_server:cast(ChannelPid, {post, UserUuid, Msg}).

% Private API

init(ChannelUuid) ->
    % Only with the process_flag below, would the terminate function
    % execute when supervisor:terminate_child is used to terminate
    % this instance.
    % process_flag(trap_exit, true),
    {ok, #{channel_uuid => ChannelUuid, members => [], messages => []}}.

handle_cast({join, UserPid},  State = #{ messages := Messages }) ->
    erlang:monitor(process, UserPid),
    lists:foreach(
        fun({_MessageUserUuid, Msg}) -> notify_user_pids([UserPid], Msg) end,
        Messages),
    {noreply, nested:update([members], fun(L) -> L ++ [#{pid => UserPid}] end, State)};

handle_cast({post, UserUuid, Msg}, State = #{ members := Members }) ->
    notify_user_pids(lists:map(fun(#{ pid := UserPid}) -> UserPid end, Members), Msg),
    {noreply, nested:update([messages], fun(Messages) -> Messages ++ [{UserUuid, Msg}] end,State)};

handle_cast(_Message,  State) ->
    io:format("casted channel spawnw"),
    {noreply, State}.


handle_call(_, _From, State) ->
    {reply, State, State}.

handle_info({'EXIT', _NonSelfPid, shutdown}, State) ->
    % https://stackoverflow.com/questions/39430574/unsupervised-gen-server-doesnt-call-terminate-when-it-receives-exit-signal
    {stop, shutdown, State};

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    ?LOG_ERROR("channel spawn DOWN ref ~p, pid ~p, ~p", [Ref, Pid, Reason]),
    {noreply, nested:update([members], fun(Members) ->
        lists:filter(fun(#{pid := UserPid}) -> Pid =/= UserPid end, Members)
    end, State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #{ uuid := Uuid }) ->
    % exit(Pid, shutdown) should trigger this function
    Pid = self(),
    user_tracker:unsubscribe(Uuid, Pid),
    ?LOG_NOTICE(#{
        reason => Reason,
        uuid => Uuid,
        self => Pid,
        notice => channel_terminated
    }),
    ok.
