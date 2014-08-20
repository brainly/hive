-module(hive_pubsub_channel).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/3, init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([publish/2, subscribe/2, unsubscribe/2, status/1, kill/2]).

-record(state, {name, prefix, clients, monitors, timeout, timer}).

-include("hive_monitor.hrl").
-define(inc(Counter), hive_monitor_utils:inc(Counter, State#state.prefix)).
-define(incby(Counter, Value), hive_monitor_utils:incby(hive_monitor_utils:name(Counter, State#state.prefix), Value)).
-define(dec(Counter), hive_monitor_utils:dec(Counter, State#state.prefix)).

%% Gen Server related:
start_link(Prefix, Name, Timeout) ->
    gen_server:start_link(?MODULE,
                          #state{prefix = Prefix,
                                 name = Name,
                                 clients = gb_sets:new(),
                                 monitors = dict:new(),
                                 timeout = Timeout,
                                 timer = undefined},
                          [{spawn_opt, [{fullsweep_after, 10}]}]).

init(State) ->
    lager:info("Starting a new Hive Pub-Sub channel ~s.", [State#state.name]),
    Timeout = case State#state.timeout of
                  0 -> infinity;
                  T -> T
              end,
    ?inc(?PUBSUB_CHANNEL_CHANNELS),
    {ok, State#state{timeout = Timeout}}.

terminate(_Reason, State) ->
    lager:info("Terminating a Hive Pub-Sub channel ~s.", [State#state.name]),
    ok.

%% External functions:
kill(Channel, Reason) ->
    gen_server:call(Channel, {try_kill, Reason}).

status(Channel) ->
    gen_server:call(Channel, status).

publish(Channel, Events) ->
    gen_server:cast(Channel, {publish, Events}).

subscribe(Channel, Pid) ->
    gen_server:call(Channel, {subscribe, Pid}).

unsubscribe(Channel, Pid) ->
    gen_server:call(Channel, {unsubscribe, Pid}).

%% Gen Server handlers:
handle_call({try_kill, Reason}, _From, State) ->
  case channel_size(State) of
       0          -> {stop, Reason, ok, State};
       _Otherwise -> {reply, {error, bussy}, State}
  end;

handle_call(status, _From, State) ->
    ?inc(?PUBSUB_CHANNEL_REQUESTS),
    ?inc(?PUBSUB_CHANNEL_STATUS),
    {reply, channel_size(State), State};

handle_call({unsubscribe, Pid}, _From, State) ->
    ?inc(?PUBSUB_CHANNEL_REQUESTS),
    ?inc(?PUBSUB_CHANNEL_UNSUBS),
    case remove_client(Pid, State, channel_size(State)) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Error} -> {reply, {error, Error}, State}
    end;

handle_call({subscribe, Pid}, _From, State) ->
    ?inc(?PUBSUB_CHANNEL_REQUESTS),
    ?inc(?PUBSUB_CHANNEL_SUBS),
    case add_client(Pid, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Error} -> {reply, {error, Error}, State}
    end;

handle_call(Action, _From, State) ->
    {reply, {error, {pubsub_channel_error, err_log("Unhandled Hive Pub-Sub Channel call: ~p",
                                                   [Action],
                                                   State)}}, State}.

handle_cast({publish, Events}, State) ->
    ?inc(?PUBSUB_CHANNEL_REQUESTS),
    ?inc(?PUBSUB_CHANNEL_PUBLISH),
    ?incby(?PUBSUB_CHANNEL_PUBLISHED, get_length(Events)),
    lager:info("Hive Pub-Sub Channel publishing events: ~p", [Events]),
    gb_sets:fold(fun(Pid, ok) ->
                         %% NOTE This bypasses the Hive Router alltogether.
                         hive_client:process_events(Pid, Events),
                         ok
                 end,
                 ok,
                 State#state.clients),
    {noreply, State};

handle_cast(Action, State) ->
    err_log("Unhandled Hive Pub-Sub Channel cast: ~p", [Action], State),
    {noreply, State}.

handle_info({timeout, _TimerRef, time_to_die}, State) ->
    %% NOTE We can't stop right away without involving hive_pubsub since
    %% NOTE this causes a race condition. Instead, we need to ask to be
    %% NOTE removed from the channel list.
    hive_pubsub:remove(self()),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case remove_client(Pid, State, channel_size(State)) of
        {ok, NewState}  -> {noreply, NewState};
        {error, _Error} -> {noreply, State}
    end;

handle_info(Info, State) ->
    err_log("Unhandled Hive Pub-Sub Channel info message: ~p", [Info], State),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    err_log("Unhandled Hive Pub-Sub Channel code change.", [], State),
    {ok, State}.

%% Internal functions:
get_length(Events) when is_list(Events) ->
    length(Events);

get_length(_) ->
    1.

add_client(Pid, State) ->
    case gb_sets:is_member(Pid, State#state.clients) of
        true ->
            %% NOTE Nothing to do, so we're good.
            {ok, State};

        false ->
            ?inc(?PUBSUB_CHANNEL_SUBSCR),
            Ref = erlang:monitor(process, Pid),
            Clients = gb_sets:add(Pid, State#state.clients),
            Monitors = dict:store(Pid, Ref, State#state.monitors),
            {ok, cancel_timer(State#state{clients = Clients, monitors = Monitors})}
    end.

remove_client(Pid, State, Size) ->
    case gb_sets:is_member(Pid, State#state.clients) of
        true ->
            ?dec(?PUBSUB_CHANNEL_SUBSCR),
            erlang:demonitor(dict:fetch(Pid, State#state.monitors)),
            Monitors = dict:erase(Pid, State#state.monitors),
            Clients = gb_sets:delete(Pid, State#state.clients),
            NewState = State#state{clients = Clients, monitors = Monitors},
            case Size of
                1 -> {ok, restart_timer(NewState)};
                _ -> {ok, NewState}
            end;

        false ->
            %% NOTE Nothing to do, so we're good.
            {ok, State}
    end.

channel_size(State) ->
    gb_sets:size(State#state.clients).

start_timer(infinity) ->
    undefined;

start_timer(Timeout) ->
    erlang:start_timer(Timeout, self(), time_to_die).

cancel_timer(State) ->
    case State#state.timer of
        undefined -> State;
        TRef      -> erlang:cancel_timer(TRef),
                     State#state{timer = undefined}
    end.

restart_timer(State) ->
    NewState = cancel_timer(State),
    NewState#state{timer = start_timer(NewState#state.timeout)}.

err_log(Format, Values, State) ->
    ?inc(?PUBSUB_CHANNEL_ERRORS),
    ErrorMsg = hive_error_utils:format(Format, Values),
    lager:warning(ErrorMsg),
    ErrorMsg.
