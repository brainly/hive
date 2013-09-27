-module(redis_connector).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(hive_plugin).
-behaviour(hive_connector).

-export([load/0, unload/1, validate/2]).
-export([common_init/2, start_pool/2, checkout/2, transaction/2, checkin/2, stop_pool/1]).

-include("hive_monitor.hrl").
-define(REDIS_CONN_COUNTERS, [?CONN_REDIS_CONNECTORS, ?CONN_REDIS_REQUESTS, ?CONN_REDIS_ERRORS, ?CONN_REDIS_QUERIES]).
-import(hive_monitor_utils, [init_counters/1, name/2]).

%% External Functions
load() ->
    {ok, [{<<"connector.redis">>, ?MODULE}], undefined}.

unload(_State) ->
    ok.

validate(<<"connector.redis">>, Descriptor) ->
    %% NOTE Built-in schema validation facility is currently unsupported in plugins.
    Schema = <<"{
    \"type\" : \"object\",
    \"properties\" : {
        \"host\" : {
            \"type\" : \"string\",
            \"required\" : true
        },
        \"port\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"maximum\" : 65535,
            \"required\" : true
        },
        \"database\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"required\" : true
        },
        \"password\" : {
            \"type\" : \"string\",
            \"required\" : true
        },
        \"restart_timeout\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"required\" : true
        },
        \"reconnect_timeout\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"required\" : true
        },
        \"max_reconnect_timeout\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"optional\" : true,
            \"default\" : 5000
        }
    }
}">>,
    Args = proplists:get_value(<<"args">>, Descriptor),
    case jesse:validate_with_schema(jsonx:decode(Schema, []), Args) of
        {ok, _Args} ->
            Host = proplists:get_value(<<"host">>, Args),
            Port = proplists:get_value(<<"port">>, Args),
            case gen_tcp:connect(binary_to_list(Host), Port, [], 5000) of
                {ok, Socket} ->
                    gen_tcp:close(Socket);

                {error, Error} ->
                    ErrorMsg = hive_error_utils:format("Unable to connect to ~s:~p: ~p!",
                                                        [Host, Port, Error]),
                    {error, ErrorMsg}
            end;

        {error, Error} ->
            ErrorMsg = hive_error_utils:prettify(<<"">>, [{<<"">>, Error}], <<",">>),
            {error, {bad_redis_connector_args, ErrorMsg}}
    end.

%% Hive connector callbacks:
common_init(PoolName, Pool) ->
    init_counters(lists:map(fun(C) -> name(C, PoolName) end, ?REDIS_CONN_COUNTERS)),
    WorkerArgs = proplists:get_value(<<"args">>, Pool),
    PoolArgs = [{worker_module, redis_worker},
                {size, proplists:get_value(<<"size">>, Pool)},
                {max_overflow, proplists:get_value(<<"overflow">>, Pool)}],
    {ok, PoolArgs, [proplists:property(pool_name, PoolName) | WorkerArgs]}.

start_pool(PoolArgs, WorkerArgs) ->
    poolboy:start_link(PoolArgs, WorkerArgs).

checkout(Pool, Timeout) ->
    Worker = poolboy:checkout(Pool, true, Timeout),
    case hive_connectors_utils:lock(Worker) of
        {error, Error} -> {error, Error};
        retry          -> Reply = checkout(Pool, Timeout),
                          poolboy:checkin(Pool, Worker),
                          Reply;
        Worker         -> {ok, Worker}
    end.

checkin(Pool, Worker) ->
    case hive_connectors_utils:unlock(Worker) of
        {error, Error} -> {error, Error};
        Worker         -> poolboy:checkin(Pool, Worker)
    end.

transaction(Pool, Transaction) ->
    poolboy:transaction(Pool, Transaction).

stop_pool(Pool) ->
    poolboy:stop(Pool).

