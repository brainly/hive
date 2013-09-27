-module(hive_hooks_client).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(hive_client_handler).

-export([init/1, terminate/2, handle/2, info/2]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-include("hive_socketio.hrl").
-include("hive_events.hrl").
-include("hive_client_handler.hrl").
-import(hive_client_state_manager, [cleanup/1]).

%% Hive Handler callbacks:
init(OriginalState) ->
    %% NOTE Since hooks can be added at runtime, there's no way to pre-initialize their Monitor counters.
    case hive_events:init() of
        {ok, EventHandlers} ->
            State = OriginalState#state{event_handlers = EventHandlers},
            case hive_hooks:init() of
                {ok, Hooks} ->
                    case hive_hooks:run(<<"on_connect">>, #sio_message{data = <<"null">>}, State#state{hooks = Hooks}) of
                        {noreply, NewState} ->
                            {ok, NewState};

                        {stop, Reason, _NewState} ->
                            ErrorMsg = hive_error_utils:format("Hive Hooks Client couldn't initialize: ~p", [Reason]),
                            lager:error(ErrorMsg),
                            {stop, {client_error, ErrorMsg}};

                        {error, Error, _NewState} ->
                            ErrorMsg = hive_error_utils:format("Hive Hooks Client couldn't initialize: ~p", [Error]),
                            lager:error(ErrorMsg),
                            {stop, {client_error, ErrorMsg}};

                        {reply, _Replies, _NewState} ->
                            ErrorMsg = hive_error_utils:format("Hive Hooks Client couldn't initialize: ~p",
                                                                [<<"tried replying at initialization.">>]),
                            lager:error(ErrorMsg),
                            {stop, {client_error, ErrorMsg}}
                    end;

                {error, Error} ->
                    ErrorMsg = hive_error_utils:format("Hive Hooks Client couldn't initialize: ~p", [Error]),
                    lager:error(ErrorMsg),
                    {stop, {client_error, ErrorMsg}}
            end;

        {error, Error} ->
            ErrorMsg = hive_error_utils:format("Hive Hooks Client couldn't initialize: ~p", [Error]),
            lager:error(ErrorMsg),
            {stop, {client_error, ErrorMsg}}
    end.

terminate(_Reason, State) ->
    %% NOTE The return value is ignored on purpose.
    hive_hooks:run(<<"on_disconnect">>, #sio_message{data = <<"null">>}, State),
    cleanup(State),
    ok.

%% Hive Handler handlers:
handle(SIOMessage = #sio_message{}, State) ->
    inc(?EXTERNAL_EVENTS),
    case SIOMessage#sio_message.type of
        event ->
            JSON = jsonx:decode(SIOMessage#sio_message.data, [{format, proplist}]),
            case hive_config:validate(<<"external_event">>, JSON) of
                {ok, Event} ->
                    EventName = proplists:get_value(<<"name">>, Event),
                    case hive_hooks:run(EventName, SIOMessage, State) of
                        {noreply, NewState} ->
                            {noreply, NewState};

                        {reply, Replies, NewState} ->
                            {reply, Replies, NewState};

                        {error, Error, NewState} ->
                            inc(?TOTAL_EVENT_ERRORS),
                            inc(?EXTERNAL_EVENT_ERRORS),
                            lager:debug("Hive Hooks Client encountered an error: ~p", [Error]),
                            {error, Error, NewState};

                        {stop, Reason, NewState} ->
                            inc(?TOTAL_EVENT_ERRORS),
                            inc(?EXTERNAL_EVENT_ERRORS),
                            lager:debug("Hive Hooks Client encountered an error: ~p", [Reason]),
                            {stop, Reason, NewState}
                    end;

                {error, Error} ->
                    inc(?TOTAL_EVENT_ERRORS),
                    inc(?EXTERNAL_EVENT_ERRORS),
                    lager:debug("Hive Hooks Client encountered an error: ~p", [Error]),
                    {error, Error, State}
            end;

        _Other ->
            inc(?TOTAL_EVENT_ERRORS),
            inc(?EXTERNAL_EVENT_ERRORS),
            ErrorMsg = hive_error_utils:format("Unhandled Hive Hooks Client external event: ~p", [SIOMessage]),
            lager:warning(ErrorMsg),
            {error, {client_error, ErrorMsg}, State}
    end.

info(Event = #internal_event{}, State) ->
    inc(?INTERNAL_EVENTS),
    case hive_events:dispatch(Event, State) of
        {noreply, NewState}      -> {noreply, NewState};
        {reply, Reply, NewState} -> {reply, Reply, NewState};
        {stop, Reason, NewState} -> inc(?TOTAL_EVENT_ERRORS),
                                    inc(?INTERNAL_EVENT_ERRORS),
                                    lager:debug("Hive Hooks Client encountered an error: ~p", [Reason]),
                                    {stop, Reason, NewState};

        {error, Error, NewState} -> inc(?TOTAL_EVENT_ERRORS),
                                    inc(?INTERNAL_EVENT_ERRORS),
                                    lager:debug("Hive Hooks Client encountered an error: ~p", [Error]),
                                    {error, Error, NewState}
    end;

info(Message, State) when is_binary(Message) ->
    inc(?INTERNAL_EVENTS),
    %% Raw send.
    %% NOTE Bypasses "event" event dispatcher but still counts towards internal events.
    {reply, Message, State};

info(Event, State) ->
    %% Some control messages used internally:
    inc(?HIVE_EVENTS),
    case Event of
        %% Convenience raw exec.
        {exec, {Module, Function, Args}} ->
            case erlang:apply(Module, Function, Args) of
                ok             -> {noreply, State};
                {error, Error} -> inc(?TOTAL_EVENT_ERRORS),
                                  inc(?HIVE_EVENT_ERRORS),
                                  lager:debug("Hive Hooks Client encountered an error: ~p", [Error]),
                                  {error, Error, State}
            end;

        %% Async error response.
        {error, Error} ->
            inc(?TOTAL_EVENT_ERRORS),
            inc(?HIVE_EVENT_ERRORS),
            lager:debug("Hive Hooks Client encountered an error: ~p", [Error]),
            {error, Error, State};

        %% Graceful termination support.
        {graceful_termination, Reason} ->
            case hive_hooks:run(<<"on_terminate">>,
                                 #sio_message{
                                     type = message,
                                     data = jsonx:encode(Reason)
                                    },
                                 State)
            of
                {noreply, NewState} ->
                    {noreply, NewState};

                {reply, Replies, NewState} ->
                    {reply, Replies, NewState};

                {error, Error, NewState} ->
                    inc(?TOTAL_EVENT_ERRORS),
                    inc(?HIVE_EVENT_ERRORS),
                    lager:debug("Hive Hooks Client encountered an error: ~p", [Error]),
                    {error, Error, NewState};

                {stop, Reason, NewState} ->
                    inc(?TOTAL_EVENT_ERRORS),
                    inc(?HIVE_EVENT_ERRORS),
                    lager:debug("Hive Hooks Client encountered an error: ~p", [Reason]),
                    {stop, Reason, NewState}
            end;

        Info ->
            inc(?TOTAL_EVENT_ERRORS),
            inc(?HIVE_EVENT_ERRORS),
            ErrorMsg = hive_error_utlis:format("Unhandled Hive Hooks Client info message: ~p", [Info]),
            lager:warning(ErrorMsg),
            {error, {client_error, ErrorMsg}, State}
    end.
