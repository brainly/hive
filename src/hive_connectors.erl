-module(hive_connectors).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/1, init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([use/1, do/2, do_safe/2, do_unsafe/2, rent/1, return/2]).
-export([start_connector/1, start_connector/4, stop_connector/1]).
-export([start_connectors/1, stop_connectors/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, dec/1]).

-record(state, {rent_timeout, pools, supervisor}).

-define(CONNECTOR_SUP_SPECS(Mod), {Mod,
                                   {Mod, start_link, []},
                                   permanent,
                                   infinity,
                                   supervisor,
                                   [Mod]}).

%% Gen Server related:
start_link(PoolSup) ->
    lager:notice("Starting Hive Connectors Manager..."),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, PoolSup, []) of
        {error, Error} -> lager:debug("Hive Connectors Manager encountered an error: ~p.", [Error]),
                          {error, Error};
        Ret            -> lager:notice("Hive Connectors Manager started!"),
                          Ret
    end.

init(PoolSup) ->
    RTimeout = hive_config:get(<<"connectors.rent_timeout">>),
    gen_server:cast(?MODULE, {start_connectors_sup, PoolSup}),
    %% NOTE Random seed is used by random backoff when performing various requests.
    %% NOTE Random backoff is used in order to inhibit unneccessary client restarts
    %% NOTE due to random pool worker errors.
    random:seed(erlang:now()),
    {ok, #state{rent_timeout = RTimeout, pools = ets:new(?MODULE, [])}}.

terminate(Reason, State) ->
    lager:notice("Hive Connectors Manager terminated:~n- State: ~p~n- Reason: ~p", [State, Reason]),
    ok.

%% External functions:
use(PoolName) ->
    inc(?CONNECTORS_REQUESTS),
    inc(?CONNECTORS_USE),
    gen_server:call(?MODULE, {use, PoolName}).

do(Pool, Transaction) ->
    do_safe(Pool, Transaction).

do_safe({connector, PoolPid, ControlerModule}, Transaction) ->
    inc(?CONNECTORS_REQUESTS),
    inc(?CONNECTORS_DO_SAFE),
    %% NOTE Delegates work to the pool returned by ?MODULE:use(PoolName)
    random_backoff(fun() -> ControlerModule:transaction(PoolPid, Transaction) end);

do_safe(PoolName, Transaction) ->
    do_safe(use(PoolName), Transaction).

do_unsafe(Pool = {connector, _PoolPid, _ControlerModule}, Transaction) ->
    %% NOTE This is an unsafe operation, as it might leak workers in case of an error.
    %% NOTE If you need to be sure that Transaction is actually a transaction, use
    %% NOTE do_safe/2 instead.
    inc(?CONNECTORS_REQUESTS),
    inc(?CONNECTORS_DO_UNSAFE),
    case rent(Pool) of
        {error, Error} ->
            {error, Error};

        {ok, Worker} ->
            Ret = Transaction(Worker),
            return(Pool, Worker),
            Ret
    end;

do_unsafe(PoolName, Transaction) ->
    do_unsafe(use(PoolName), Transaction).

rent({connector, PoolPid, ControlerModule}) ->
    inc(?CONNECTORS_REQUESTS),
    inc(?CONNECTORS_RENT),
    RTimeout = hive_config:get(<<"connectors.rent_timeout">>), %% FIXME This could be faster...
    %% NOTE Delegates work to the pool returned by ?MODULE:use(PoolName)
    random_backoff(fun() -> ControlerModule:checkout(PoolPid, RTimeout) end);

rent(PoolName) ->
    rent(use(PoolName)).

return({connector, PoolPid, ControlerModule}, Worker) ->
    inc(?CONNECTORS_REQUESTS),
    inc(?CONNECTORS_RETURN),
    %% NOTE Delegates work to the pool returned by ?MODULE:use(PoolName)
    random_backoff(fun() -> ControlerModule:checkin(PoolPid, Worker) end);

return(PoolName, Worker) ->
    return(use(PoolName), Worker).

stop_connector(Name) ->
    inc(?CONNECTORS_REQUESTS),
    inc(?CONNECTORS_STOP),
    gen_server:cast(?MODULE, {stop_connector, Name}).

start_connector({PoolName, PoolDescriptor}) ->
    inc(?CONNECTORS_REQUESTS),
    inc(?CONNECTORS_SPAWN),
    gen_server:call(?MODULE, {start_connector, PoolName, PoolDescriptor}).

stop_connectors(Names) ->
    lists:foreach(fun stop_connector/1, Names),
    ok.

start_connectors(Descriptors) ->
    lists:foldl(fun ({Name, Descriptor}, ok) ->
                        start_connector({Name, Descriptor});

                    ({_Name, _Descriptor}, Error) ->
                        Error
                end,
                ok,
                Descriptors).

%% Internal-but-still-exported functions:
start_connector(PoolName, Controler, PoolArgs, WorkerArgs) ->
    %% NOTE This is used by the pool supervisor to spawn new pools.
    lager:notice("Starting Hive Connector ~s...", [PoolName]),
    case Controler:start_pool(PoolArgs, WorkerArgs) of
        {error, Error} -> inc(?CONNECTORS_ERRORS),
                          ErrorMsg = hive_error_utils:format("Unable to start Hive Connector ~s: ~p",
                                                             [PoolName, Error]),
                          lager:error(ErrorMsg),
                          {error, {connectors_error, ErrorMsg}};
        Ret            -> lager:notice("Hive Connector ~s started!", [PoolName]),
                          Ret
    end.

%% Gen Server handlers:
handle_call({use, PoolName}, _From, State) ->
    case lookup(PoolName, State) of
        undefined -> {reply, {error, {bad_connector_id, PoolName}}, State};
        P         -> {reply, {connector, P, controler_module(P, State)}, State}
    end;

handle_call({start_connector, PoolName, PoolDescriptor}, _From, State) ->
    {reply, do_start_connector(PoolName, PoolDescriptor, State), State};

handle_call(Action, _From, State) ->
    inc(?CONNECTORS_ERRORS),
    lager:warning("Unhandled Hive Connectors Server call: ~p", [Action]),
    {noreply, State}.

handle_cast({stop_connector, PoolName}, State) ->
    case ets:lookup(State#state.pools, PoolName) of
        [{PoolName, RealName}] ->
            lager:notice("Stopping Hive Connectors pool ~s...", [PoolName]),
            stop_pool(PoolName, State),
            dec(?CONNECTORS_POOLS),
            ets:delete(State#state.pools, PoolName),
            ets:delete(State#state.pools, RealName),
            lager:notice("Hive Connectors pool ~s stopped!", [PoolName]),
            {noreply, State};

        [] ->
            inc(?CONNECTORS_ERRORS),
            lager:warning("Hive Connectors Manager tried stopping an unknown pool: ~p", [PoolName]),
            {noreply, State}
    end;

handle_cast({start_connectors_sup, PoolSup}, State) ->
    case supervisor:start_child(PoolSup, ?CONNECTOR_SUP_SPECS(hive_connectors_pool_sup)) of
        {ok, Pid} ->
            start_predefined_connectors(Pid, State#state{supervisor = Pid});

        {error, {already_started, Pid}}->
            inc(?CONNECTORS_ERRORS),
            lager:warning("Hive Connectors Supervisor already started!"),
            start_predefined_connectors(Pid, State#state{supervisor = Pid});

        {error, Error} ->
            inc(?CONNECTORS_ERRORS),
            lager:debug("Hive Connectors Manager encountered an error: ~p", [Error]),
            {stop, Error, State}
    end;

handle_cast(Action, State) ->
    inc(?CONNECTORS_ERRORS),
    lager:warning("Unhandled Hive Connectors Manager cast: ~p", [Action]),
    {noreply, State}.

handle_info(Info, State) ->
    inc(?CONNECTORS_ERRORS),
    lager:warning("Unhandled Hive Connectors Manager info message: ~p", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    inc(?CONNECTORS_ERRORS),
    lager:warning("Unhandled Hive Connectors Manager code change."),
    {ok, State}.

%% Internal functions:
lookup(Pool, State) when is_binary(Pool) ->
    case ets:lookup(State#state.pools, Pool) of
        [{Pool, PoolName}] -> PoolName;
        []                 -> inc(?CONNECTORS_ERRORS),
                              lager:warning("Tried accessing an unknown Connector: ~p", [Pool]),
                              undefined
    end;

lookup(Pool, _State) when is_atom(Pool) orelse is_pid(Pool) ->
    Pool.

controler_module(Pool, State) when is_binary(Pool) ->
    controler_module(lookup(Pool, State), State);

controler_module(Pool, State) ->
    case ets:lookup(State#state.pools, Pool) of
        []                          -> inc(?CONNECTORS_ERRORS),
                                       lager:warning("Tried type-checking an unknown Connector: ~p", [Pool]),
                                       undefined; %% FIXME Things probably can go boom.
        [{_RealName, ControlerModule}] -> ControlerModule
    end.

do_start_connector(PoolName, PoolDescriptor, State) ->
    case hive_config:validate(<<"connector">>, PoolDescriptor) of
        {ok, _Descriptor} ->
            case init_pool(PoolName, PoolDescriptor) of
                {ok, Controler, PoolArgs, WorkerArgs} ->
                    case supervisor:start_child(State#state.supervisor,
                                                [PoolName, Controler, PoolArgs, WorkerArgs])
                    of
                        {ok, Name} ->
                            RealName = case Name of
                                           {golbal, N}       -> N;
                                           {N, _Node}        -> N;
                                           {via, _Module, N} -> N;
                                           N                 -> N
                                       end,
                            inc(?CONNECTORS_POOLS),
                            ets:insert(State#state.pools, {PoolName, RealName}),
                            ets:insert(State#state.pools, {RealName, Controler}),
                            ok;

                        {error, Error} ->
                            inc(?CONNECTORS_ERRORS),
                            lager:debug("Hive Connectors Manager encountered an error: ~p", [Error]),
                            {error, Error}
                    end;

                {error, Error} ->
                    inc(?CONNECTORS_ERRORS),
                    lager:debug("Hive Connectors Manager encountered an error: ~p", [Error]),
                    {error, Error}
            end;

        {error, Error} ->
            inc(?CONNECTORS_ERRORS),
            lager:debug("Hive Connectors Manager encountered an error: ~p", [Error]),
            {error, Error}
    end.

init_pool(Name, Pool) ->
    case hive_plugins:get(proplists:get_value(<<"connector">>, Pool)) of
        {ok, Controler} ->
            case Controler:common_init(Name, Pool) of
                {stop, Reason} ->
                    inc(?CONNECTORS_ERRORS),
                    ErrorMsg = hive_error_utils:format("Unable to initialize Hive Connector ~s: ~p",
                                                       [Pool, Reason]),
                    lager:error(ErrorMsg),
                    {error, {connectors_error, ErrorMsg}};

                {ok, PArgs, WArgs} ->
                    {ok, Controler, PArgs, WArgs}
            end;

        {error, Error} ->
            inc(?CONNECTORS_ERRORS),
            lager:debug("Hive Connectors Manager encountered an error: ~p", [Error]),
            {error, Error}
    end.

start_predefined_connectors(Sup, State) ->
    case lists:foldl(fun({PoolName, PoolDescriptor}, ok) ->
                             do_start_connector(PoolName, PoolDescriptor, State);

                        (_, Error) ->
                             Error
                     end,
                     ok,
                     hive_config:get(<<"connectors.pools">>))
    of
        ok             -> {noreply, State#state{supervisor = Sup}};
        {error, Error} -> {stop, Error, State}
    end.

stop_pool(PoolName, State) ->
    Pool = lookup(PoolName, State),
    ControlerModule = controler_module(Pool, State),
    ControlerModule:stop(Pool).

random_backoff(Fun) ->
    random_backoff(hive_config:get(<<"connectors.backoff_num">>, 1),
                   hive_config:get(<<"connectors.backoff_time">>, 0),
                   Fun).

random_backoff(1, _Timeout, Fun) ->
    Fun();

random_backoff(Times, Timeout, Fun) ->
    case Fun() of
        Ret = {error, Error} -> inc(?CONNECTORS_ERRORS),
                                lager:debug("Hive Connectors Manager encountered an error and will retry: ~p", [Error]),
                                RandomTimeout = random:uniform(Timeout),
                                timer:sleep(RandomTimeout),
                                random_backoff(Times - 1, Ret, Fun);
        Otherwise            -> Otherwise
    end.
