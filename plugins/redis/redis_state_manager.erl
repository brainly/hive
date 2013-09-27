-module(redis_state_manager).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(hive_plugin).

-export([load/0, unload/1, validate/2]).

-include("hive_client_handler.hrl").
-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

%% External functions:
load() ->
    {ok, [{<<"sm.redis">>, fun init/1}], undefined}.

unload(_State) ->
    ok.

validate(<<"sm.redis">>, Descriptor) ->
    Args = proplists:get_value(<<"args">>, Descriptor),
    Connector = proplists:get_value(<<"connector">>, Args),
    Timeout = proplists:get_value(<<"expiration_timeout">>, Args),
    case is_integer(Timeout) andalso Timeout > 0 of
        true ->
            case hive_config:connector(Connector) of
                {ok, ConnectorDescriptor} ->
                    case proplists:get_value(<<"connector">>, ConnectorDescriptor) of
                        <<"connector.redis">> ->
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
            ErrorMsg = hive_error_utils:format("Invalid expiration timeout: ~p", [Timeout]),
            {error, {bad_config, ErrorMsg}}
    end.

%% Internal functions:
init(State) ->
    Sid = State#state.sid,
    InitialState = State#state.real_state,
    Args = State#state.args,
    case init_state(Sid, InitialState, Args) of
        {ok, RealState} ->
            {ok, State#state{
                   getter = fun get/2,
                   setter = fun set/3,
                   cleanup = fun cleanup/1,
                   real_state = RealState
                  }};

        Other ->
            Other
    end.

init_state(Sid, List = [{_Key, _Value} | _Rest], Args) ->
    Pool = proplists:get_value(<<"connector">>, Args),
    case set(all, List, #state{sid = Sid, args = Args}) of
        {ok, _Value1, State} ->
            case proplists:get_value(<<"expiration_timeout">>, Args) of
                undefined ->
                    {ok, undefined};

                Timeout ->
                    case do(Pool, ["EXPIRE", Sid, Timeout div 1000], State) of
                        {ok, _Value2, State}  -> {ok, undefined};
                        {error, Error, State} -> {stop, Error};
                        {stop, Reason, State} -> {stop, Reason}
                    end
            end;

        {error, Error} ->
            lager:debug("Hive Redis State Manager encountered an error: ~p", [Error]),
            {stop, Error}
    end;

init_state(Sid, Value, Args) ->
    init_state(Sid, [{<<"initial_value">>, Value}], Args).

get(all, State) ->
    case State#state.real_state of
        undefined ->
            Sid = State#state.sid,
            Pool = proplists:get_value(<<"connector">>, State#state.args),
            case do(Pool, ["HGETALL", Sid], State) of
                {ok, Value, State} -> Cached = fold_deserialize(Value),
                                      {ok, Cached, State#state{real_state = Cached}};
                Other              -> Other
            end;

        Cached ->
            {ok, Cached, State}
    end;

get(What, State) ->
    Sid = State#state.sid,
    Pool = proplists:get_value(<<"connector">>, State#state.args),
    do(Pool, ["HGET", Sid, What], State).

set(all, Values, State) ->
    Sid = State#state.sid,
    Pool = proplists:get_value(<<"connector">>, State#state.args),
    %% NOTE We reset the Redis cache.
    do(Pool, ["HMSET", Sid | unfold_serialize(Values)], State#state{real_state = undefined});

set(What, Value, State) ->
    Sid = State#state.sid,
    Pool = proplists:get_value(<<"connector">>, State#state.args),
    do(Pool, ["HSET", Sid, What, serialize(Value)], State#state{real_state = undefined}).

cleanup(State) ->
    Sid = State#state.sid,
    Pool = proplists:get_value(<<"connector">>, State#state.args),
    do(Pool, ["DEL", Sid], State).

%% Internal functions:
do(Pool, Query, State) ->
    case hive_connectors:do(Pool,
                             fun(Redis) ->
                                     redis_worker:q(Redis, Query)
                             end)
    of
        {ok, Value} ->
            {ok, Value, State};

        {error, {no_connection, Error}} ->
            inc(?STATE_MGR_ERRORS),
            lager:debug("Hive Redis State Manager encountered an error: ~p",
                        [{redis_error, Error}]),
            {stop, {shutdown, {redis_error, Error}}, State};

        {error, Error} ->
            inc(?STATE_MGR_ERRORS),
            lager:debug("Hive Redis State Manager encountered an error: ~p", [Error]),
            {error, Error, State}
    end.

fold_deserialize([A, B | Rest]) ->
    [{A, deserialize(B)} | fold_deserialize(Rest)];

fold_deserialize([]) ->
    [].

unfold_serialize([{A, B} | Rest]) ->
    [A, serialize(B) | unfold_serialize(Rest)];

unfold_serialize([]) ->
    [].

serialize(Value) when is_integer(Value) ->
    Value;

serialize(Value) when is_binary(Value) ->
    Value;

serialize(Value) ->
    jsonx:encode(Value).

deserialize(Value) ->
    case jsonx:decode(Value, [{format, proplist}]) of
        {error, invalid_json, _} -> Value;
        Result -> Result
    end.
