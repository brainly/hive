-module(hive_websocket_client).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_fsm).

-export([init/1, terminate/3]).
-export([transient/2, transient/3, polling/2, polling/3, waiting/2, waiting/3]).
-export([handle_info/3, handle_event/3, handle_sync_event/4, code_change/4]).

-include("hive_client.hrl").
-import(hive_client_utils, [start_session_timer/1, reset_session_timer/1]).
-import(hive_client_utils, [cancel_poll_timer/1, reset_poll_timer/1]).
-import(hive_client_utils, [start_heartbeat_timer/1, reset_heartbeat_timer/1]).
-import(hive_client_utils, [dispatch_events/2, dispatch_messages/2, log_transition/2]).
-import(hive_client_utils, [handler_handle_info/6, terminate_sink/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, dec/1]).

%% Gen FSM callbacks
init(State) ->
    Handler = State#state.handler,
    case Handler:init(State#state.client_state) of
        {ok, ClientState} ->
            inc(?WS_CLIENT_NUM),
            NewState = State#state{client_state = ClientState},
            case NewState#state.sink of
                undefined ->
                    log_transition(generic, transient),
                    {ok, transient, start_heartbeat_timer(start_session_timer(NewState))};

                Sink ->
                    log_transition(generic, waiting),
                    {ok, waiting, start_heartbeat_timer(start_session_timer(initialize_sink(Sink, NewState)))}
            end;

        {stop, Reason} ->
            inc(?CLIENT_ERRORS),
            lager:debug("Hive Client encountered an error: ~p", [Reason]),
            {stop, Reason}
    end.

terminate(Reason, StateName, State) ->
    %% NOTE Flushes any remaining messages and terminates the client.
    flush_messages(State, false),
    Handler = State#state.handler,
    ClientState = State#state.client_state,
    Handler:terminate(Reason, ClientState),
    terminate_sink(State),
    dec(?WS_CLIENT_NUM),
    log_transition(StateName, generic),
    ok.

%% External API
%% NOTE Defined in the generic hive_client.

%% Gen FSM handlers
transient({set_sink, Sink}, State) ->
    log_transition(transient, waiting),
    {next_state, waiting, initialize_sink(Sink, State)};

transient({timeout, _Ref, Type}, State) ->
    handle_timeout(Type, transient, State);

transient(Event, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client async event: ~p", [Event]),
    {next_state, transient, State}.

transient(Event, _From, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client sync event: ~p", [Event]),
    {next_state, transient, State}.

waiting({dispatch_messages, Messages}, State) ->
    case dispatch_messages(Messages, reset_session_timer(State)) of
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};

        {error, _Error, NewState} ->
            %% NOTE The error is already being queued for sending.
            log_transition(waiting, polling),
            {next_state, polling, reset_poll_timer(NewState)};

        {reply, NewState} ->
            log_transition(waiting, polling),
            {next_state, polling, reset_poll_timer(NewState)};

        {noreply, NewState} ->
            {next_state, waiting, NewState}
    end;

waiting({dispatch_events, Events}, State) ->
    case dispatch_events(Events, State) of
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};

        {error, _Error, NewState} ->
            %% NOTE The error is already being queued for sending.
            log_transition(waiting, polling),
            {next_state, polling, reset_poll_timer(NewState)};

        {reply, NewState} ->
            log_transition(waiting, polling),
            {next_state, polling, reset_poll_timer(NewState)};

        {noreply, NewState} ->
            {next_state, waiting, NewState}
    end;

waiting({timeout, _Ref, Type}, State) ->
    handle_timeout(Type, waiting, State);

waiting(Event, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client async event: ~p", [Event]),
    {next_state, waiting, State}.

waiting(Event, _From, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client sync event: ~p", [Event]),
    {next_state, waiting, State}.

polling({dispatch_messages, Messages}, State) ->
    case dispatch_messages(Messages, reset_session_timer(State)) of
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};

        {error, _Error, NewState} ->
            %% NOTE The error is already being queued for sending.
            {next_state, polling, NewState};

        {_Ok, NewState} ->
            {next_state, polling, NewState}
    end;

polling({dispatch_events, Events}, State) ->
    case dispatch_events(Events, State) of
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};

        {error, _Error, NewState} ->
            %% NOTE The error is already being queued for sending.
            {next_state, polling, NewState};

        {_Ok, NewState} ->
            {next_state, polling, NewState}
    end;

polling({timeout, _Ref, Type}, State) ->
    handle_timeout(Type, polling, State);

polling(Event, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client async event: ~p", [Event]),
    {next_state, polling, State}.

polling(Event, _From, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client sync event: ~p", [Event]),
    {next_state, polling, State}.

handle_event({terminate, Reason}, _StateName, State) ->
    {stop, Reason, State};

handle_event(Event, StateName, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client async event: ~p", [Event]),
    {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client sync event: ~p", [Event]),
    {next_state, StateName, State}.

handle_info(Info, waiting, State) ->
    handler_handle_info(Info,
                        fun(Reason, NewState) -> {stop, Reason, NewState} end,
                        fun(NewState)         -> log_transition(waiting, polling),
                                                 {next_state, polling, reset_poll_timer(NewState)} end,
                        fun(NewState)         -> log_transition(waiting, polling),
                                                 {next_state, polling, reset_poll_timer(NewState)} end,
                        fun(NewState)         -> {next_state, waiting, NewState} end,
                        State);

handle_info(Info, StateName, State) ->
    handler_handle_info(Info,
                        fun(Reason, NewState) -> {stop, Reason, NewState} end,
                        fun(NewState)         -> {next_state, StateName, NewState} end,
                        fun(NewState)         -> {next_state, StateName, NewState} end,
                        fun(NewState)         -> {next_state, StateName, NewState} end,
                        State).

code_change(_OldVsn, StateName, State, _Extra) ->
    inc(?CLIENT_ERRORS),
    lager:warning("Unhandled Hive Client code change."),
    {ok, StateName, State}.

%% Internal functions
handle_timeout(poll_timeout, StateName, State) ->
    NewState = flush_messages(State, false),
    log_transition(StateName, waiting),
    {next_state, waiting, NewState};

handle_timeout(heartbeat_timeout, StateName, State) ->
    NewState = flush_messages(State, true),
    log_transition(StateName, waiting),
    {next_state, waiting, NewState};

handle_timeout(session_timeout, _StateName, State) ->
    inc(?CLIENT_ERRORS),
    ErrorMsg = hive_error_utils:format("Client session timed out, closing connection."),
    lager:error(ErrorMsg),
    {stop, {shutdown, {session_timeout, ErrorMsg}}, State}.

flush_messages(State, _SendEmpty = true) ->
    flush(reset_heartbeat_timer(cancel_poll_timer(State)));

flush_messages(State, _SendEmpty = false) ->
    case State#state.messages of
        []        -> cancel_poll_timer(State);
        _Messages -> flush(reset_heartbeat_timer(cancel_poll_timer(State)))
    end.

flush(State) ->
    case State#state.sink of
        undefined -> inc(?CLIENT_ERRORS),
                     ErrorMsg = hive_error_utils:format("Tried sending a batch of messages before a sink was set. Aborting."),
                     %% NOTE When there's no sink set, we simply assume the Client disconnected in a weird way.
                     lager:warning(ErrorMsg),
                     hive_client:terminate(self(), {shutdown, ErrorMsg});
        Sink      -> Sink ! {reply, lists:map(fun(Message) ->
                                                      hive_socketio_parser:encode(Message)
                                              end,
                                              State#state.messages)}
    end,
    State#state{messages = []}.

initialize_sink(Sink, State) ->
    case hive_config:get(<<"hive.direct_websocket">>, false) of
        true -> register_self(Sink);
        _    -> ok
    end,
    State#state{sink = Sink}.

register_self(Sink) ->
    lager:info("Registering Client ~p with the sink...", [self()]),
    Sink ! {register, self()}.
