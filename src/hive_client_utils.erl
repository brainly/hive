-module(hive_client_utils).
-author('kajetan.rzepecki@zadane.pl').

-export([start_session_timer/1, cancel_session_timer/1, reset_session_timer/1]).
-export([start_poll_timer/1, cancel_poll_timer/1, reset_poll_timer/1]).
-export([start_heartbeat_timer/1, cancel_heartbeat_timer/1, reset_heartbeat_timer/1]).
-export([dispatch_events/2, dispatch_messages/2, log_transition/2, combine_replies/2]).
-export([handler_handle_info/6, terminate_sink/1]).

-include("hive_client.hrl").
-include("hive_socketio.hrl").
-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, dec/1]).

%% External Functions:
cancel_session_timer(State) ->
    case State#state.session_timer of
        undefined -> ok;
        T         -> gen_fsm:cancel_timer(T)
    end,
    State#state{session_timer = undefined}.

start_session_timer(State) ->
    case State#state.session_timer of
        undefined -> T = gen_fsm:start_timer(State#state.session_timeout, session_timeout),
                     State#state{session_timer = T};
        _         -> State
    end.

reset_session_timer(State) ->
    start_session_timer(cancel_session_timer(State)).

cancel_poll_timer(State) ->
    case State#state.poll_timer of
        undefined -> ok;
        T         -> gen_fsm:cancel_timer(T)
    end,
    State#state{poll_timer = undefined}.

start_poll_timer(State) ->
    case State#state.poll_timer of
        undefined -> T = gen_fsm:start_timer(State#state.poll_timeout, poll_timeout),
                     State#state{poll_timer = T};
        _         -> State
    end.

reset_poll_timer(State) ->
    start_poll_timer(cancel_poll_timer(State)).

cancel_heartbeat_timer(State) ->
    case State#state.hb_timer of
        undefined -> ok;
        T         -> gen_fsm:cancel_timer(T)
    end,
    State#state{hb_timer = undefined}.

start_heartbeat_timer(State) ->
    case State#state.hb_timer of
        undefined -> T = gen_fsm:start_timer(State#state.heartbeat_timeout, heartbeat_timeout),
                     State#state{hb_timer = T};
        _         -> State
    end.

reset_heartbeat_timer(State) ->
    start_heartbeat_timer(cancel_heartbeat_timer(State)).

dispatch_events(Events, State) when is_list(Events) ->
    lists:foldl(fun (Event, Acc) ->
                        dispatch(State#state.handler, info, Event, Acc)
                end,
                {noreply, State},
                Events);

dispatch_events(Event, State) ->
    dispatch_events([Event], State).

dispatch_messages(Messages, State) when is_list(Messages) ->
    lists:foldl(fun (Msg, Acc) ->
                        dispatch(State#state.handler, handle, Msg, Acc)
                end,
                {noreply, State},
                Messages);

dispatch_messages(Event, State) ->
    dispatch_messages([Event], State).

dispatch(Handler, Fun, Message, {Ok, State}) ->
    inc(?TOTAL_EVENTS),
    ClientState = State#state.client_state,
    case Handler:Fun(Message, ClientState) of
        {stop, Reason, NewClientState} ->
            inc(?CLIENT_ERRORS),
            lager:debug("Hive Client encountered an error: ~p", [Reason]),
            Reply = hive_error_utils:make_reply(Reason),
            Messages = State#state.messages,
            NewState = State#state{messages = combine_replies(Reply, Messages),
                                   client_state = NewClientState},
            {stop, Reason, NewState};

        {error, Error, NewClientState} ->
            inc(?CLIENT_ERRORS),
            lager:debug("Hive Client encountered an error: ~p", [Error]),
            Reply = hive_error_utils:make_reply(Error),
            Messages = State#state.messages,
            NewState = State#state{messages = combine_replies(Reply, Messages),
                                   client_state = NewClientState},
            {error, Error, NewState};

        {reply, Reply, NewClientState} ->
            Messages = State#state.messages,
            NewState = State#state{messages = combine_replies(Reply, Messages),
                                   client_state = NewClientState},
            {reply, NewState};

        {noreply, NewClientState} ->
            {Ok, State#state{client_state = NewClientState}}
    end;

dispatch(_Handler, _Fun, _Message, Error) ->
    Error.

handler_handle_info(Info, OnStop, OnError, OnReply, OnNoReply, State) ->
    case dispatch(State#state.handler, info, Info, {noreply, State}) of
        {stop, Reason, NewState} ->
            OnStop(Reason, NewState);

        {error, _Error, NewState} ->
            OnError(NewState);

        {reply, NewState} ->
            OnReply(NewState);

        {noreply, NewState} ->
            OnNoReply(NewState)
    end.

terminate_sink(State) ->
    case State#state.sink of
        undefined -> inc(?CLIENT_ERRORS),
                     lager:warning("Tried closing a sink before it was set.");
        Sink      -> Sink ! terminate
    end.

log_transition(State, State) ->
    ok;

log_transition(From, To) ->
    inc(?TRANSITIONS_NUM),
    log_transition_from(From),
    log_transition_to(To).

log_transition_from(generic) ->
    dec(?GENERIC_NUM);

log_transition_from(transient) ->
    dec(?TRANSIENT_NUM);

log_transition_from(waiting) ->
    dec(?WAITING_NUM);

log_transition_from(polling) ->
    dec(?POLLING_NUM).

log_transition_to(generic) ->
    inc(?GENERIC_NUM);

log_transition_to(transient) ->
    inc(?TRANSIENT_NUM);

log_transition_to(waiting) ->
    inc(?WAITING_NUM);

log_transition_to(polling) ->
    inc(?POLLING_NUM).

combine_replies(A, B) when is_list(A) andalso is_list(B) ->
    A ++ B;
combine_replies(A, B) when is_list(A) ->
    [B | A];
combine_replies(A, B) when is_list(B) ->
    [A | B];
combine_replies(A, B) ->
    [A, B].

