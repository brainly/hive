-module(redis_worker).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([stop/1, q/2, q/3, qp/2, qp/3, qn/2]).

-include("hive_connectors.hrl").
-import(hive_connectors_utils, [restart_timer/1, cancel_timer/1]).

-record(data, {host         :: list(),
               port         :: integer(),
               database     :: integer(),
               password     :: list(),
               eredis_state :: term()}).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/2, dec/2]).
-define(inc(Counter), inc(Counter, State#state.pool_name)).
-define(dec(Counter), dec(Counter, State#state.pool_name)).

-define(DEFAULT_TIMEOUT, infinity). %% Default request timeou.

%% A fat Eredis wrapper
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    erlang:process_flag(trap_exit, true), %% NOTE This is required in order to clean up properly.
    Host = binary_to_list(proplists:get_value(<<"host">>, Args)),
    Port = proplists:get_value(<<"port">>, Args),
    Database = proplists:get_value(<<"database">>, Args),
    Password = binary_to_list(proplists:get_value(<<"password">>, Args)),
    RestartTimeout = proplists:get_value(<<"restart_timeout">>, Args),
    ReconnectTimeout = proplists:get_value(<<"reconnect_timeout">>, Args),
    MaxReconnectTimeout = proplists:get_value(<<"max_reconnect_timeout">>, Args, 5000),
    Name = proplists:get_value(pool_name, Args),
    State = #state{lock = unlocked,
                   pool_name = Name,
                   reconnect_timeout = ReconnectTimeout,
                   max_reconnect_timeout = MaxReconnectTimeout,
                   restart_timeout = RestartTimeout,
                   restart_timer = undefined},
    case eredis_client:init([Host, Port, Database, Password, ReconnectTimeout]) of
        {ok, EredisState} ->
            Data = #data{host = Host,
                         port = Port,
                         database = Database,
                         password = Password,
                         eredis_state = EredisState},
            ?inc(?CONN_REDIS_CONNECTORS),
            {ok, restart_timer(State#state{data = Data})};

        {stop, Reason} ->
            {stop, Reason}
    end.

terminate(_Reason, State) ->
    ?dec(?CONN_REDIS_CONNECTORS),
    ok.

%% External API:
stop(Client) ->
    gen_server:cast(Client, stop).

q(Client, Command) ->
    q(Client, Command, ?DEFAULT_TIMEOUT).

q(Client, Command, Timeout) ->
    eredis:q(Client, Command, Timeout).

qp(Client, Commands) ->
    qp(Client, Commands, ?DEFAULT_TIMEOUT).

qp(Client, Commands, Timeout) ->
    eredis:qp(Client, Commands, Timeout).

qn(Client, Command) ->
    eredis:q_noreply(Client, Command).

%% Gen Server handlers:
handle_call(lock, _From, State) ->
    hive_connectors_utils:lock(State);

handle_call(unlock, _From, State) ->
    hive_connectors_utils:unlock(State);

handle_call(R = {request, _Req}, From, State) ->
    ?inc(?CONN_REDIS_QUERIES),
    handle_eredis_client_call(R, From, State);

handle_call(Request, From, State) ->
    handle_eredis_client_call(Request, From, State).

handle_cast(stop, State) ->
    hive_connectors_utils:stop(State);

handle_cast(R = {request, _Req}, State) ->
    ?inc(?CONN_REDIS_QUERIES),
    handle_eredis_client_cast(R, State);

handle_cast(Msg, State) ->
    ?inc(?CONN_REDIS_ERRORS),
    lager:warning("Unhandled Hive Redis Connector cast: ~p", [Msg]),
    {noreply, State}.

handle_info({timeout, _Ref, {try_connect_loop, Request, From, TimeLeft}}, State = #state{reconnect_timeout = Timeout})
  when TimeLeft >= Timeout ->
    %% NOTE This is a part of an eraborate, transparent reconnection loop.
    %% NOTE Eredis Client module handles actual Redis reconnection, while
    %% NOTE this module tries to re-run the offending request.
    Data = State#state.data,
    case eredis_client:handle_call(Request, From, Data#data.eredis_state) of
        {reply, {error, no_connection}, NewEredisState} ->
            ?inc(?CONN_REDIS_ERRORS),
            ErrorMsg = <<"Hive Redis Connector could not establish a connection to the Redis database! Retrying...">>,
            lager:warning(ErrorMsg),
            erlang:start_timer(Timeout, self(), {try_connect_loop, Request, From, TimeLeft - Timeout}),
            {noreply, State#state{data = Data#data{eredis_state = NewEredisState}}};

        Otherwise ->
            dispatch(Request, Otherwise, State, From)
    end;

handle_info({timeout, _Ref, {try_connect_loop, Request, From, _TimeLeft}}, State) ->
    dispatch(Request, {reply, {error, no_connection}, State#state.data#data.eredis_state}, State, From);

handle_info({'EXIT', _Pid, Reason}, State) ->
    %% NOTE Since we use the trap_exit flag, it'll receive a few messages we don't want to log.
    {stop, Reason, State};

handle_info({timeout, _Ref, stop}, State) ->
    stop(self()),
    {noreply, cancel_timer(State)};

handle_info(Info, State) ->
    handle_eredis_client_info(Info, State).

code_change(_OldVsn, State, _Extra) ->
    ?inc(?CONN_REDIS_ERRORS),
    lager:warning("Unhandled Hive Redis Connector code change."),
    {ok, State}.

%% Internal functions:
handle_eredis_client_call(Request, From, State) ->
    ?inc(?CONN_REDIS_REQUESTS),
    dispatch(Request, try_connect(Request, From, State), State).

handle_eredis_client_cast(Request, State) ->
    ?inc(?CONN_REDIS_REQUESTS),
    dispatch(Request, eredis_client:handle_cast(Request, State#state.data#data.eredis_state), State).

handle_eredis_client_info(Request, State) ->
    dispatch(Request, eredis_client:handle_info(Request, State#state.data#data.eredis_state), State).

dispatch(Request, Reply, State) ->
    dispatch(Request, Reply, State, fun (R, S, Mod) -> {reply, R, S, Mod} end).

dispatch(Request, Result, State, ReplyFun) when is_function(ReplyFun) ->
    Data = State#state.data,
    case Result of
        {reply, unknown_request, NewEredisState} ->
            ?inc(?CONN_REDIS_ERRORS),
            ErrorMsg = hive_error_utils:format("Unhandled Hive Redis Connector call: ~p", [Request]),
            lager:warning(ErrorMsg),
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            ReplyFun({error, {redis_error, ErrorMsg}}, NewState);

        {reply, Reply, NewEredisState} ->
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            ReplyFun(dispatch_reply(Reply, NewState), NewState, infinity);

        {reply, Reply, NewEredisState, hibernate} ->
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            ReplyFun(dispatch_reply(Reply, NewState), NewState, hibernate);

        {reply, Reply, NewEredisState, Timeout} ->
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            ReplyFun(dispatch_reply(Reply, NewState), NewState, Timeout);

        {noreply, NewEredisState} ->
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            {noreply, NewState};

        {noreply, NewEredisState, hibernate} ->
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            {noreply, NewState, hibernate};

        {noreply, NewEredisState, Timeout} ->
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            {noreply, NewState, Timeout};

        {stop, {unhandled_message, Info}, NewEredisState} ->
            ?inc(?CONN_REDIS_ERRORS),
            lager:warning("Unhandled Hive Redis Connector info: ~p.", [Info]),
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            {noreply, NewState};

        {stop, Reason, NewEredisState} ->
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            {stop, Reason, NewState};

        {stop, Reason, Reply, NewEredisState} ->
            NewState = State#state{data = Data#data{eredis_state = NewEredisState}},
            {stop, Reason, Reply, NewState}
    end;

dispatch(Request, Reply, State, From) ->
    dispatch(Request, Reply, State, fun(R, S, Mod) -> gen_server:reply(From, R), {noreply, S, Mod} end).

dispatch_reply({error, no_connection}, State) ->
    ?inc(?CONN_REDIS_ERRORS),
    ErrorMsg = <<"Hive Redis Connector could not establish a connection to the Redis database!">>,
    lager:warning(ErrorMsg),
    {error, {no_connection, ErrorMsg}};

dispatch_reply({error, Error}, State) ->
    ?inc(?CONN_REDIS_ERRORS),
    ErrorMsg = hive_error_utils:format("Hive Redis Connectors request failed: ~p", [Error]),
    lager:warning(ErrorMsg),
    {error, {redis_error, ErrorMsg}};

dispatch_reply(Reply, _State) ->
    Reply.

try_connect(Request, From, State) ->
    Timeout = State#state.reconnect_timeout,
    TimeLeft = State#state.max_reconnect_timeout,
    erlang:start_timer(10, self(), {try_connect_loop, Request, From, TimeLeft + Timeout}),
    {noreply, State#state.data#data.eredis_state}.
