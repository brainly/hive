-module(hive_client).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_fsm).

-export([start_link/2, init/1, terminate/3]).
-export([generic/2, generic/3]).
-export([handle_info/3, handle_event/3, handle_sync_event/4, code_change/4]).
-export([set_sink/2, process_messages/2, terminate/2, upgrade/2, process_events/2, kill/2]).

-include("hive_client.hrl").
-import(hive_client_utils, [cancel_session_timer/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, dec/1]).

%% Gen FSM callbacks
start_link(Sid, Handler) ->
    gen_fsm:start_link(?MODULE, {Sid, Handler}, [{spawn_opt, [{fullsweep_after, 10}]}]).

init({Sid, Handler}) ->
    %% NOTE Initialization timer is specialcased here not to create any additional state fields.
    InitTimeout = hive_config:get(<<"socketio.init_timeout">>),
    SessionTimeout = hive_config:get(<<"socketio.session_timeout">>),
    PollTimeout = hive_config:get(<<"socketio.poll_timeout">>),
    HeartbeatTimeout = hive_config:get(<<"socketio.heartbeat_timeout">>),
    InitTimer = gen_fsm:start_timer(InitTimeout, init_timeout),
    StateDescription = hive_config:get(<<"clients.state">>),
    State = #state{handler = Handler,
                   session_timer = InitTimer,
                   %% NOTE Stores these not to invoke application:get_env all the time later.
                   session_timeout = SessionTimeout,
                   poll_timeout = PollTimeout,
                   heartbeat_timeout = HeartbeatTimeout},
    case hive_client_state_manager:init_state(Sid, StateDescription) of
        {ok, InitialState} ->
            inc(?CLIENT_NUM),
            inc(?GENERIC_NUM),
            {ok, generic, State#state{client_state = InitialState}};

        {stop, Reason} ->
            inc(?CLIENT_ERRORS),
            lager:debug("Hive Client encountered an error: ~p", [Reason]),
            {stop, Reason}
    end.

terminate(Reason, _StateName, _State) ->
    dec(?GENERIC_NUM),
    dec(?CLIENT_NUM),
    lager:debug("Hive Client terminated with reason: ~p", [Reason]),
    ok.

%% External API
-type sink() :: pid().
-type proto_spec() :: 'websocket' | 'xhr_polling'.

-spec upgrade(Pid :: pid(), ProtocolSpec :: proto_spec()) -> ok.
-spec set_sink(Pid :: pid(), Sink :: sink()) -> ok.
-spec process_events(Pid :: pid(), Event :: [term()] | term()) -> ok.
-spec process_messages(Pid :: pid(), Messages :: [term()] | term()) -> ok.
-spec terminate(Pid :: pid(), Reason :: term()) -> ok.
-spec kill(Pid :: pid(), Reason :: term()) -> ok.

upgrade(Pid, ProtocolSpec) ->
    gen_fsm:send_event(Pid, {upgrade, ProtocolSpec}).

set_sink(Pid, Sink) ->
    gen_fsm:send_event(Pid, {set_sink, Sink}).

process_events(Pid, Events) ->
    gen_fsm:send_event(Pid, {dispatch_events, Events}).

process_messages(Pid, Messages) ->
    gen_fsm:send_event(Pid, {dispatch_messages, Messages}).

terminate(Pid, Reason) ->
    gen_fsm:send_all_state_event(Pid, {terminate, Reason}).

kill(Pid, Reason) ->
    erlang:kill(Pid, {shutdown, Reason}).

%% Gen FSM handlers:
generic({upgrade, websocket}, State) ->
    lager:info("Upgrading generic client to WebSocket!"),
    case hive_websocket_client:init(cancel_session_timer(State)) of
        {ok, StateName, FSMState} ->
            gen_fsm:enter_loop(hive_websocket_client, [], StateName, FSMState);

        {stop, Reason} ->
            inc(?CLIENT_ERRORS),
            lager:error("Unable to upgrade the client to WebSocket: ~p", [Reason]),
            {stop, Reason, State}
    end;

generic({upgrade, xhr_polling}, State) ->
    lager:info("Upgrading generic client to XHR-polling!"),
    case hive_xhr_polling_client:init(cancel_session_timer(State)) of
        {ok, StateName, FSMState} ->
            gen_fsm:enter_loop(hive_xhr_polling_client, [], StateName, FSMState);

        {stop, Reason} ->
            inc(?CLIENT_ERRORS),
            lager:error("Unable to upgrade the client to XHR-polling: ~p", [Reason]),
            {stop, Reason, State}
    end;

generic({timeout, _Timer, init_timeout}, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Client failed to initialize in time!"),
    {stop, shutdown, State};

generic({set_sink, Sink}, State) ->
    Sink ! finalize_handshake,
    %% NOTE It'll store the sink for a later upgrade.
    {next_state, generic, State#state{sink = Sink}};

generic(Event, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client async event: ~p", [Event]),
    {next_state, generic, State}.

generic(Event, _From, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client synch event: ~p", [Event]),
    {next_state, generic, State}.

handle_event(Event, StateName, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client async event: ~p", [Event]),
    {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client sync event: ~p", [Event]),
    {next_state, StateName, State}.

handle_info(Event, StateName, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client info: ~p", [Event]),
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client code change."),
    {ok, StateName, State}.
