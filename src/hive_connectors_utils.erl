-module(hive_connectors_utils).
-author('kajetan.rzepecki@zadane.pl').

-export([lock/1, unlock/1, stop/1]).
-export([cancel_timer/1, start_timer/1, restart_timer/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-include("hive_connectors.hrl").

lock(State = #state{}) ->
    case State#state.lock of
        %% NOTE We can't lock an already locked worker.
        locked   -> {reply, {error, not_locked}, State};
        %% NOTE We can't use this worker, because it's stopping.
        stopped  -> {reply, {error, stopped}, State};
        unlocked -> {reply, {ok, locked}, State#state{lock = locked}}
    end;

lock(Pid) ->
    case catch gen_server:call(Pid, lock) of
        {ok, locked} ->
            %% NOTE The worker is now locked.
            Pid;

        {error, stopped} ->
            %% NOTE The worker is stopping, so we can't use it.
            inc(?CONNECTORS_ERRORS),
            ErrorMsg = "Unable to lock a Hive Connector worker: worker is stopping.",
            {error, {connector_error, ErrorMsg}};

        {error, not_locked} ->
            %% NOTE The worker is already locked, so we can't use it.
            inc(?CONNECTORS_ERRORS),
            ErrorMsg = "Unable to lock a Hive Connector worker: worker already locked.",
            {error, {connector_error, ErrorMsg}};

        _ ->
            %% NOTE The worker is terminated, we should retry.
            retry
    end.

unlock(State = #state{}) ->
    case State#state.lock of
        %% NOTE We can't unlock an already unlocked worker.
        unlocked -> {reply, {error, not_unlocked}, State};
        %% NOTE The worker is supposed to stop, so we terminate it ASAP.
        stopped  -> {stop, shutdown, {ok, stopped}, State};
        locked   -> {reply, {ok, unlocked}, State#state{lock = unlocked}}
    end;

unlock(Pid) ->
    case catch gen_server:call(Pid, unlock) of
        {ok, unlocked} ->
            %% NOTE The worker is now locked.
            Pid;

        {ok, stopped} ->
            %% NOTE The worker just stopped.
            Pid;

        {error, not_unlocked} ->
            %% NOTE The worker is already locked, so we can't use it.
            inc(?CONNECTORS_ERRORS),
            ErrorMsg = "Unable to unlock a Hive Connector worker: worker not locked.",
            {error, {connector_error, ErrorMsg}};

        _ ->
            %% NOTE The worker is terminated.
            inc(?CONNECTORS_ERRORS),
            ErrorMsg = "Unable to unlock a Hive Connector worker: worker terminated.",
            {error, {connector_error, ErrorMsg}}
    end.

stop(State) ->
    case State#state.lock of
        %% NOTE We can't stop now, because somebody is using this worker.
        locked   -> {noreply, State#state{lock = locked}};
        %% NOTE We are already stopping and we'll terminate as soon, as this worker is returned.
        stopped  -> {noreply, State#state{lock = stopped}};
        unlocked -> {stop, shutdown, State}
    end.

cancel_timer(State) ->
    case State#state.restart_timer of
        undefined -> ok;
        T         -> erlang:cancel_timer(T)
    end,
    State#state{restart_timer = undefined}.

start_timer(State) ->
    Timer = case State#state.restart_timer of
                undefined -> erlang:start_timer(State#state.restart_timeout, self(), stop);
                T         -> T
            end,
    State#state{restart_timer = Timer}.

restart_timer(State) ->
    start_timer(cancel_timer(State)).

