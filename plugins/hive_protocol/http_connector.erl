-module(http_connector).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).
-behaviour(hive_plugin).
-behaviour(hive_connector).

-export([load/0, unload/1, validate/2]).
-export([common_init/2, start_pool/2, checkout/2, transaction/2, checkin/2, stop_pool/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-include("hive_monitor.hrl").
-define(HTTP_CONN_COUNTERS, [?CONN_HTTP_CONNECTORS, ?CONN_HTTP_REQUESTS, ?CONN_HTTP_ERRORS, ?CONN_HTTP_GET,
                             ?CONN_HTTP_POST, ?CONN_HTTP_APOST]).
-import(hive_monitor_utils, [init_counters/1, name/2]).
-define(inc(Counter), hive_monitor_utils:inc(Counter, State#state.pool_name)).
-define(dec(Counter), hive_monitor_utils:dec(Counter, State#state.pool_name)).

-include("hive_connectors.hrl").

%% Hive plugin callbacks:
load() ->
    {ok, [{<<"connector.http">>, ?MODULE}], undefined}.

unload(_State) ->
    ok.

validate(<<"connector.http">>, Descriptor) ->
    %% NOTE Built-in schema validation facility is currently unsupported in plugins.
    Schema = <<"{
    \"type\" : \"object\",
    \"properties\" : {
            \"base_url\" : {
            \"type\" : \"string\",
            \"required\" : true
        },
        \"max_connection_timeout\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"required\" : true
        },
        \"max_connections\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"optional\" : true
        }
    }
}">>,
    Args = proplists:get_value(<<"args">>, Descriptor),
    case jesse:validate_with_schema(jsonx:decode(Schema), Args) of
        {ok, _Args} ->
            Url = proplists:get_value(<<"base_url">>, Args),
            UrlList = binary_to_list(Url),
            {ok, {http, _Info, Host, Port, _Endpoint, _Query}} = http_uri:parse(UrlList),
            case gen_tcp:connect(Host, Port, [], 5000) of
                {ok, Socket} ->
                    gen_tcp:close(Socket),
                    ok;

                {error, Error} ->
                    ErrorMsg = hive_error_utils:format("Unable to connect to ~s:~p: ~p!",
                                                        [Host, Port, Error]),
                    {error, ErrorMsg}
            end;

        {error, Error} ->
            ErrorMsg = hive_error_utils:prettify(<<"">>, [{<<"">>, Error}], <<",">>),
            {error, {bad_http_connector_args, ErrorMsg}}
    end.

%% Hive connector callbacks:
common_init(PoolName, Pool) ->
    init_counters(lists:map(fun(C) -> name(C, PoolName) end, ?HTTP_CONN_COUNTERS)),
    WorkerArgs = proplists:get_value(<<"args">>, Pool),
    MaxConnectors = proplists:get_value(<<"size">>, Pool) + proplists:get_value(<<"overflow">>, Pool),
    Max = proplists:get_value(<<"max_connections">>, WorkerArgs, MaxConnectors),
    Url = proplists:get_value(<<"base_url">>, WorkerArgs),
    case http_uri:parse(binary_to_list(Url)) of
        {ok, {_Scheme, _User, Host, Port, _Path, _Query}} ->
            ibrowse:set_max_sessions(Host, Port, Max),
            {ok, [], [proplists:property(pool_name, PoolName) | WorkerArgs]};

        {error, Error} ->
            State = #state{pool_name = PoolName},
            ?inc(?CONN_HTTP_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive HTTP Connector ~s is unable to initialize: ~p",
                                                [PoolName, Error]),
            lager:error(ErrorMsg),
            {stop, {http_error, ErrorMsg}}
    end.

start_pool([], WorkerArgs) ->
    gen_server:start_link(?MODULE, WorkerArgs, []).

checkout(Pool, Timeout) ->
    gen_server:call(Pool, {checkout, Timeout}).

checkin(Pool, Worker) ->
    gen_server:call(Pool, {checkin, Worker}).

transaction(Pool, Transaction) ->
    gen_server:call(Pool, {transaction, Transaction}).

stop_pool(Pool) ->
    gen_server:call(Pool, stop).

%% Gen server callbacks:
init(WorkerArgs) ->
    PoolName = proplists:get_value(pool_name, WorkerArgs),
    Sink = spawn(fun ignore/0),
    State = #state{pool_name = PoolName, data = {WorkerArgs, Sink}},
    ?inc(?CONN_HTTP_CONNECTORS),
    {ok, State}.

terminate(_Reason, State) ->
    ?dec(?CONN_HTTP_CONNECTORS),
    ok.

handle_call({checkout, _Timeout}, _From, State) ->
    %% NOTE We return the connector state which will then be used by the worker code.
    {reply, {ok, {http_worker, State}}, State};

handle_call({checkin, _Worker}, _From, State) ->
    {reply, ok, State};

handle_call({transaction, Transaction}, _From, State) ->
    {reply, Transaction(State), State};

handle_call(stop, _From, State) ->
    {stop, shutdown, State};

handle_call(Message, _From, State) ->
    ?inc(?CONN_HTTP_ERRORS),
    lager:warning("Unhandled Hive HTTP Connector cast: ~p", [Message]),
    {reply, ok, State}.

handle_cast(Message, State) ->
    ?inc(?CONN_HTTP_ERRORS),
    lager:warning("Unhandled Hive HTTP Connector cast: ~p", [Message]),
    {noreply, State}.

handle_info(Info, State) ->
    ?inc(?CONN_HTTP_ERRORS),
    lager:warning("Unhandled Hive HTTP Connector info: ~p", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    ?inc(?CONN_HTTP_ERRORS),
    lager:warning("Unhandled Hive HTTP Connector code change."),
    {ok, State}.

ignore() ->
    %% NOTE This tiny function is used as a common sink for async Ibrowse replies.
    %% NOTE It's created only once and it's shared between all the requests.
    receive
        _ -> ignore()
    end.
