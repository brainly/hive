-module(tcp_connector).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).
-behaviour(hive_connector).
-behaviour(hive_plugin).

-export([load/0, unload/1, validate/2]).
-export([common_init/2, start_pool/2, checkout/2, transaction/2, checkin/2, stop_pool/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([remove/2, add/2]).

-include("hive_connectors.hrl").

-record(tcp_state, {
          listener,
          pool_name,
          froms,
          workers,
          workers_queue
         }).

-include("hive_monitor.hrl").
-define(TCP_CONN_COUNTERS, [?CONN_TCP_CONNECTORS, ?CONN_TCP_REQUESTS, ?CONN_TCP_ERRORS, ?CONN_TCP_SEND,
                            ?CONN_TCP_RECV]).

-import(hive_monitor_utils, [inc/2, init_counters/1, name/2]).
-define(inc(Counter), inc(Counter, State#tcp_state.pool_name)).
-define(dec(Counter), dec(Counter, State#tcp_state.pool_name)).

%% External Functions
load() ->
    {ok, [{<<"connector.tcp">>, ?MODULE}], undefined}.

unload(_State) ->
    ok.

validate(<<"connector.tcp">>, Descriptor) ->
    %% NOTE Built-in schema validation facility is currently unsupported in plugins.
    Schema = <<"{
    \"type\" : \"object\",
    \"properties\" : {
        \"port\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"maximum\" : 65535,
            \"required\" : true
        },
        \"restart_timeout\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"required\" : true
        },
        \"max_connection_timeout\" : {
            \"type\" : \"integer\",
            \"minimum\" : 0,
            \"optional\" : true,
            \"default\" : 5000
        }
    }
}">>,
    Args = proplists:get_value(<<"args">>, Descriptor),
    case jesse:validate_with_schema(jsonx:decode(Schema), Args) of
        {ok, _Args} ->
            %% NOTE TCP conncetor is used to listen on the hive side. It's assumed to be fine.
            ok;

        {error, Error} ->
            ErrorMsg = hive_error_utils:prettify(<<"">>, [{<<"">>, Error}], <<",">>),
            {error, {bad_tcp_connector_args, ErrorMsg}}
    end.

%% Hive connector callbacks:
common_init(PoolName, Pool) ->
    init_counters(lists:map(fun(C) -> name(C, PoolName) end, ?TCP_CONN_COUNTERS)),
    WorkerArgs = proplists:get_value(<<"args">>, Pool),
    {ok, Pool, [proplists:property(pool_name, PoolName) | WorkerArgs]}.

start_pool(PoolArgs, WorkerArgs) ->
    gen_server:start_link(?MODULE, {PoolArgs, WorkerArgs}, []).

checkout(Pool, Timeout) ->
    gen_server:call(Pool, {checkout, Timeout}).

checkin(Pool, Worker) ->
    gen_server:cast(Pool, {checkin, Worker}).

transaction(Pool, Transaction) ->
    gen_server:call(Pool, {transaction, Transaction}).

stop_pool(Pool) ->
    gen_server:call(Pool, stop).

%% Internal TCP connector functions:
remove(Pool, Worker) ->
    gen_server:call(Pool, {remove, Worker}).

add(Pool, Worker) ->
    gen_server:cast(Pool, {add, Worker}).

%% Gen Server callbacks:
init({PoolArgs, WorkerArgs}) ->
    process_flag(trap_exit, true), %% NOTE In order to clean up properly.
    PoolName = proplists:get_value(pool_name, WorkerArgs),
    Size = proplists:get_value(<<"size">>, PoolArgs),
    Port = proplists:get_value(<<"port">>, WorkerArgs),
    State = #tcp_state{
               pool_name = PoolName,
               workers = [],
               workers_queue = queue:new(),
               froms = queue:new()
              },
    case ranch:start_listener(PoolName, Size,
                              ranch_tcp, [{port, Port}],
                              tcp_worker, [self(), WorkerArgs])
    of
        {ok, _Pid} ->
            {ok, State#tcp_state{listener = PoolName}};

        {error, {already_started, _Pid}} ->
            ranch:set_protocol_options(PoolName, [self(), WorkerArgs]),
            ?inc(?CONN_TCP_ERRORS),
            lager:warning("Hive TCP Connector listener already started!"),
            {ok, State#tcp_state{listener = PoolName}};

        {error, Error} ->
            ?inc(?CONN_TCP_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive TCP Connector ~s is unable to initialize the listener: ~p",
                                                [PoolName, Error]),
            lager:error(ErrorMsg),
            {stop, {tcp_error, ErrorMsg}}
    end.

terminate(_Reason, State) ->
    ranch:stop_listener(State#tcp_state.listener),
    stop_workers(State),
    stop_requests(State).

handle_call({checkout, Timeout}, From, State) ->
    case checkout_worker(State) of
        retry_later ->
            Timer = erlang:start_timer(Timeout, self(), {checkout_timeout, From}),
            Queue = State#tcp_state.froms,
            {noreply, State#tcp_state{froms = queue:in({From, Timer}, Queue)}};

        {ok, Worker, NewState} ->
            {reply, {ok, Worker}, NewState};

        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call({transaction, Transaction}, From, State) ->
    case checkout_worker(State) of
        retry_later ->
            Queue = State#tcp_state.froms,
            {noreply, State#tcp_state{froms = queue:in({From, Transaction}, Queue)}};

        {ok, Worker, NewState} ->
            Reply = Transaction(Worker),
            case checkin_worker(Worker, NewState) of
                {ok, NewestState} ->
                    {reply, Reply, NewestState};

                {error, Error} ->
                    {reply, {error, Error}, State}
            end;

        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call({remove, Worker}, _From, State) ->
    {reply, ok, remove_worker(Worker, State)};

handle_call(stop, _From, State) ->
    {stop, shutdown, State};

handle_call(Message, _From, State) ->
    ?inc(?CONN_TCP_ERRORS),
    lager:warning("Unhandled Hive TCP Connector call: ~p", [Message]),
    {reply, ok, State}.

handle_cast({checkin, Worker}, State) ->
    case checkin_worker(Worker, State) of
        {ok, NewState} ->
            {noreply, NewState};

        {error, Error} ->
            lager:error("Hive TCP Connector encountered an error: ~p", [Error]),
            {noreply, State}
    end;

handle_cast({add, Worker}, State) ->
    Workers = State#tcp_state.workers,
    case checkin_worker(Worker, State#tcp_state{workers = [Worker | Workers]}) of
        {ok, NewState} ->
            {noreply, NewState};

        {error, Error} ->
            lager:error("Hive TCP Connector encountered an error: ~p", [Error]),
            {noreply, State}
    end;

handle_cast(Message, State) ->
    ?inc(?CONN_TCP_ERRORS),
    lager:warning("Unhandled Hive TCP Connector cast: ~p", [Message]),
    {noreply, State}.

handle_info({timeout, _Ref, {checkout_timout, From}}, State) ->
    PoolName = State#tcp_state.pool_name,
    ?inc(?CONN_TCP_ERRORS),
    ErrorMsg = hive_error_utils:format("Hive TCP Connector ~s's request timed out!", [PoolName]),
    lager:error(ErrorMsg),
    gen_server:reply(From, {error, {tcp_error, ErrorMsg}}),
    NewFroms = queue:filter(fun({F, _T}) -> F =/= From end, State#tcp_state.froms),
    {noreply, State#tcp_state{froms = NewFroms}};

handle_info({'EXIT', Worker, _Reason}, State) ->
    {noreply, remove_worker(Worker, State)};

handle_info(Info, State) ->
    ?inc(?CONN_TCP_ERRORS),
    lager:warning("Unhandled Hive TCP Connector info: ~p", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    ?inc(?CONN_TCP_ERRORS),
    lager:warning("Unhandled Hive TCP Connector code change."),
    {ok, State}.

%% Internal functions:
checkout_worker(State) ->
    Queue = State#tcp_state.workers_queue,
    case queue:out(Queue) of
        {{value, Worker}, NewQueue} ->
            {ok, Worker, State#tcp_state{workers_queue = NewQueue}};

        {empty, Queue} ->
            retry_later
    end.

checkin_worker(Worker, State) ->
    Froms = State#tcp_state.froms,
    case queue:out(Froms) of
        %% NOTE We have an ongoing transaction which has to be carried out...
        {{value, {From, Transaction}}, NewFroms} when is_function(Transaction, 1) ->
            %% FIXME This shouldn't run here as it may clog the entire connector up.
            gen_server:reply(From, Transaction(Worker)),
            checkin_worker(Worker, State#tcp_state{froms = NewFroms});

        %% NOTE ...or an ongoing checkout request...
        {{value, {From, Timer}}, NewFroms} ->
            erlang:cancel_timer(Timer),
            gen_server:reply(From, {ok, Worker}),
            {ok, State#tcp_state{froms = NewFroms}};

        %% NOTE ...or we're good to go.
        {empty, Froms} ->
            Queue = State#tcp_state.workers_queue,
            {ok, State#tcp_state{workers_queue = queue:in(Worker, Queue)}}
    end.

stop_requests(State) ->
    PoolName = State#tcp_state.pool_name,
    lists:foreach(fun({From, _Transaction}) ->
                          ?inc(?CONN_TCP_ERRORS),
                          ErrorMsg = hive_error_utils:format("Hive TCP Connector ~s's request failed!",
                                                              [PoolName]),
                          lager:error(ErrorMsg),
                          gen_server:reply(From, {error, {tcp_error, ErrorMsg}})
                  end,
                  queue:to_list(State#tcp_state.froms)).

stop_workers(State) ->
    lists:foreach(fun(Worker) ->
                          tcp_worker:stop(Worker)
                  end,
                  State#tcp_state.workers).

remove_worker(Worker, State) ->
    NewQueue = queue:filter(fun(W) -> W =/= Worker end, State#tcp_state.workers_queue),
    NewWorkers = lists:filter(fun(W) -> W =/= Worker end, State#tcp_state.workers),
    State#tcp_state{workers = NewWorkers, workers_queue = NewQueue}.
