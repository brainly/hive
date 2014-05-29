-module(hp_hook).
-author('kajetan.rzepecki@zadane.pl').

-export([load/0, unload/1, validate/2]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, inc/2]).

%% External functions:
load() ->
    {ok, [{<<"hp.post">>, fun init_post/1},
          {<<"hp.get">>, fun init_get/1},
          {<<"hp.put">>, fun init_put/1}], undefined}.

unload(_State) ->
    ok.

validate(_Name, Descriptor) ->
    Args = proplists:get_value(<<"args">>, Descriptor),
    Connector = proplists:get_value(<<"connector">>, Args),
    Endpoint = proplists:get_value(<<"endpoint">>, Args),
    case is_binary(Endpoint) of
        true ->
            case hive_config:connector(Connector) of
                {ok, ConnectorDescriptor} ->
                    case proplists:get_value(<<"connector">>, ConnectorDescriptor) of
                        <<"connector.http">> ->
                            ok;

                        <<"connector.tcp">> ->
                            ok;

                        Type ->
                            ErrorMsg = hive_error_utils:format("Connector ~s is of invalid type: ~s",
                                                                [Connector, Type]),
                            {error, {bad_config, ErrorMsg}}
                    end;

                {error, {Code, _Error}} ->
                    ErrorMsg = hive_error_utils:format("Undefined connector ~s", [Connector]),
                    {error, {Code, ErrorMsg}}
            end;

        false ->
            ErrorMsg = hive_error_utils:format("Invalid endpoint: ~p", [Endpoint]),
            {error, {bad_config, ErrorMsg}}
    end.

%% Internal functions:
init_get(_Args) ->
    {ok, fun get/4}.

get(Args, Event, _Trigger, State) ->
    inc(?HOOK_HP_GET),
    inc(?HOOK_EVENT_HP_GET, Event),
    Endpoint = proplists:get_value(<<"endpoint">>, Args),
    do(fun(Worker) ->
               hive_protocol:get(Worker, Endpoint)
       end,
       Event,
       Args,
       State).

init_post(_Args) ->
    {ok, fun post/4}.

post(Args, Event, Trigger, State) ->
    inc(?HOOK_HP_POST),
    inc(?HOOK_EVENT_HP_POST, Event),
    Endpoint = proplists:get_value(<<"endpoint">>, Args),
    case hive_hooks_utils:state_json(Trigger, State) of
        {ok, Data, NewState} ->
            do(fun(Worker) ->
                       hive_protocol:post(Worker, Endpoint, Data)
               end,
               Event,
               Args,
               NewState);

        {error, Error, NewState} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Protocol Hook encountered an error: ~p", [Error]),
            {error, Error, NewState};

        {stop, Reason, NewState} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Protocol Hook encountered an error: ~p", [Reason]),
            {stop, Reason, NewState}
    end.

init_put(_Args) ->
    {ok, fun put/4}.

put(Args, Event, Trigger, State) ->
    inc(?HOOK_HP_PUT),
    inc(?HOOK_EVENT_HP_PUT, Event),
    Endpoint = proplists:get_value(<<"endpoint">>, Args),
    case hive_hooks_utils:state_json(Trigger, State) of
        {ok, Data, NewState} ->
            do(fun(Worker) ->
                       hive_protocol:put(Worker, Endpoint, Data)
               end,
               Event,
               Args,
               NewState);

        {error, Error, NewState} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Protocol Hook encountered an error: ~p", [Error]),
            {error, Error, NewState};

        {stop, Reason, NewState} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Protocol Hook encountered an error: ~p", [Reason]),
            {stop, Reason, NewState}
    end.

%% Utility functions:
do(Fun, Event, Args, State) ->
    Connector = proplists:get_value(<<"connector">>, Args),
    case hive_connectors:do_unsafe(Connector, Fun) of
        {error, retry_later} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Protocol Hook encountered an error: ~p", [retry_later]),
            do(Fun, Event, Args, State);

        {error, Error} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Protocol Hook encountered an error: ~p", [Error]),
            {error, Error, State};

        {ok, Reply} ->
            case hive_events:parse(Reply) of
                {ok, Events} ->
                    hive_events:dispatch(Events, State);

                {error, Error} ->
                    inc(?HOOK_ERRORS),
                    inc(?HOOK_EVENT_ERRORS, Event),
                    lager:debug("Hive Protocol Hook encountered an error: ~p", [Error]),
                    {error, Error, State}
            end;

        ok ->
            {noreply, State}
    end.
