-module(hive_router).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/1, init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-export([new_client/1, upgrade_client/2, kill_client/1]).
-export([route_message/2, route_messages/2, set_sink/2]).
-export([route_event/2, route_events/2]).
-export([uptime/0, msg_queue/0]).
-export([terminate_after/2, enable/0, disable/0]).

-record(state, {ready, supervisor, clients, start_time}).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, incby/2, dec/1]).

-define(POOL_SUP_SPEC(Mod), {Mod,
                             {Mod, start_link, []},
                             permanent,
                             infinity,
                             supervisor,
                             [Mod]}).

%% Gen Server related:
start_link(PoolSup) ->
    lager:notice("Starting Hive Router..."),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, PoolSup, [{spawn_opt, [{priority, high}]}]) of
        {error, Error} -> lager:debug("Hive Router encountered an error: ~p.", [Error]),
                          {error, Error};
        Ret            -> lager:notice("Hive Router started!"),
                          Ret
    end.

init(PoolSup) ->
    StartTime = hive:uptime(),
    gen_server:cast(?MODULE, {start_client_sup, PoolSup}),
    {ok, #state{clients = ets:new(?MODULE, []), ready = true, start_time = StartTime}}.

terminate(Reason, State) ->
    lager:notice("Hive Router terminated: ~n- State: ~p~n- Reason: ~p", [State, Reason]),
    ets:delete(State#state.clients),
    ok.

%% External functions:
-type sid() :: any().
-type protocol() :: 'websocket' | 'xhr_polling'.

-spec new_client(Sid :: sid()) -> any().
-spec upgrade_client(Sid :: sid(), Protocol :: protocol()) -> any().
-spec kill_client(Sid :: sid()) -> any().
-spec route_message(Sid :: sid(), Message :: any()) -> any().
-spec route_messages(Sid :: sid(), Messages :: [any()]) -> any().
-spec set_sink(Sid :: sid(), Sink :: pid() | any()) -> any().

new_client(Handler) ->
    inc(?ROUTER_REQUESTS),
    gen_server:call(?MODULE, {new_client, Handler}).

upgrade_client(Sid, Protocol) ->
    inc(?ROUTER_REQUESTS),
    gen_server:call(?MODULE, {upgrade, Sid, Protocol}).

kill_client(Sid) ->
    inc(?ROUTER_REQUESTS),
    gen_server:call(?MODULE, {kill_client, Sid}).

route_message(Sid, Message) ->
    inc(?ROUTER_REQUESTS),
    route_messages(Sid, Message).

route_messages(Sid, Messages) ->
    inc(?ROUTER_REQUESTS),
    gen_server:call(?MODULE, {route_messages, Sid, Messages}).

route_event(Sid, Event) ->
    inc(?ROUTER_REQUESTS),
    route_events(Sid, Event).

route_events(Sid, Events) ->
    inc(?ROUTER_REQUESTS),
    gen_server:call(?MODULE, {route_events, Sid, Events}).

set_sink(Sid, Sink) ->
    inc(?ROUTER_REQUESTS),
    gen_server:call(?MODULE, {set_sink, Sid, Sink}).

%% Other API calls:
-spec terminate_after(Time :: integer(), Reason :: any()) -> ok.
-spec enable() -> ok.
-spec disable() -> ok.

terminate_after(Time, Reason) ->
    inc(?ROUTER_REQUESTS),
    gen_server:call(?MODULE, {terminate, Time, Reason}).

enable() ->
    inc(?ROUTER_REQUESTS),
    gen_server:call(?MODULE, {toggle, true}).

disable() ->
    inc(?ROUTER_REQUESTS),
    gen_server:call(?MODULE, {toggle, false}).

%% Other external functions, mainly for the Hive Monitor.
-spec uptime() -> integer().
-spec msg_queue() -> integer().

uptime() ->
    %% NOTE No need to increment the ROUTER_REQUESTS counter, since it's not really a reuest.
    gen_server:call(?MODULE, uptime).

msg_queue() ->
    gen_server:call(?MODULE, msg_queue).

%% Gen Server handlers:
handle_call({new_client, Handler}, _From, State) ->
    {reply, spawn_client(State, [Handler]), State};

handle_call(uptime, _From, State) ->
    {reply, hive:uptime() - State#state.start_time, State};

handle_call(msg_queue, _From, State) ->
    {reply, element(2, erlang:process_info(self(), message_queue_len)), State};

handle_call({route_events, Sid, Events}, _From, State) ->
    case get_client(Sid, State#state.clients) of
        unknown -> inc(?ROUTER_ERRORS),
                   ErrorMsg = hive_error_utils:format("Tried routing events to an unknown sid: ~s", [Sid]),
                   lager:warning(ErrorMsg),
                   {reply, {error, {bad_session_id, ErrorMsg}}, State};
        Pid     -> incby(?ROUTED_EVENTS, get_len(Events)),
                   hive_client:process_events(Pid, Events),
                   {reply, ok, State}
    end;

handle_call({route_messages, Sid, Messages}, _From, State) ->
    case get_client(Sid, State#state.clients) of
        unknown -> inc(?ROUTER_ERRORS),
                   ErrorMsg = hive_error_utils:format("Tried routing messages to an unknown sid: ~s", [Sid]),
                   lager:warning(ErrorMsg),
                   {reply, {error, {bad_session_id, ErrorMsg}}, State};
        Pid     -> incby(?ROUTED_MSGS, get_len(Messages)),
                   hive_client:process_messages(Pid, Messages),
                   {reply, ok, State}
    end;

handle_call({set_sink, Sid, NewSink}, _From, State) ->
    case get_client(Sid, State#state.clients) of
        unknown -> inc(?ROUTER_ERRORS),
                   ErrorMsg = hive_error_utils:format("Tried setting the sink of an unknown sid: ~s", [Sid]),
                   lager:warning(ErrorMsg),
                   {reply, {error, {bad_session_id, ErrorMsg}}, State};
        Pid     -> hive_client:set_sink(Pid, NewSink),
                   {reply, ok, State}
    end;

handle_call({upgrade, Sid, Protocol}, _From, State) ->
    case get_client(Sid, State#state.clients) of
        unknown -> inc(?ROUTER_ERRORS),
                   ErrorMsg = hive_error_utils:format("Tried upgrading an unknown sid: ~s", [Sid]),
                   lager:warning(ErrorMsg),
                   {reply, {error, {bad_session_id, ErrorMsg}}, State};
        Pid     -> hive_client:upgrade(Pid, Protocol),
                   {reply, ok, State}
    end;

handle_call({kill_client, Sid}, _From, State) ->
    case get_client(Sid, State#state.clients) of
        unknown -> inc(?ROUTER_ERRORS),
                   ErrorMsg = hive_error_utils:format("Tried killing an unknown sid: ~s", [Sid]),
                   lager:warning(ErrorMsg),
                   {reply, {error, {bad_session_id, ErrorMsg}}, State};
        Pid     -> hive_client:terminate(Pid, shutdown),
                   {reply, ok, State}
    end;

handle_call({toggle, OnOff}, _From, State) ->
    lager:notice("Hive Router is toggled ~s.", [case OnOff of
                                                     true  -> "on";
                                                     false -> "off"
                                                 end]),
    {reply, ok, State#state{ready = OnOff}};

handle_call({terminate, Time,  Reason}, _From, State) ->
    lager:notice("Hive Router is terminating clients: ~p", [Reason]),
    erlang:start_timer(Time, self(), {terminate, Reason}),
    ets:foldl(fun({Pid, _Sid}, ok) when is_pid(Pid) ->
                      %% NOTE Notify about graceful termination...
                      hive_client:process_events(Pid, {graceful_termination, Reason}),
                      %% NOTE ...request immediate termination.
                      hive_client:terminate(Pid, {shutdown, Reason}),
                      ok;

                 (_Otherwise, ok) ->
                      ok
              end,
              ok,
              State#state.clients),
    {reply, ok, State#state{ready = false}};

handle_call(Action, _From, State) ->
    inc(?ROUTER_ERRORS),
    ErrorMsg = hive_error_utils:format("Unhandled Hive Router call: ~p", [Action]),
    {reply, {error, {router_error, ErrorMsg}}, State}.

handle_cast({start_client_sup, PoolSup}, State) ->
    case supervisor:start_child(PoolSup, ?POOL_SUP_SPEC(hive_client_sup)) of
        {ok, Pid} ->
            {noreply, State#state{supervisor = Pid}};

        {error, {already_started, Pid}}->
            inc(?ROUTER_ERRORS),
            lager:warning("Hive Client Supervisor already started!"),
            {noreply, State#state{supervisor = Pid}};

        {error, Reason} ->
            inc(?ROUTER_ERRORS),
            lager:debug("Hive Router encountered an error: ~p", [Reason]),
            {stop, Reason, State}
    end;

handle_cast(Action, State) ->
    inc(?ROUTER_ERRORS),
    lager:warning("Unhandled Hive Router cast: ~p", [Action]),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    remove_client(Pid, State#state.clients, Reason),
    {noreply, State};

handle_info({timeout, _Ref, {terminate, Reason}}, State) ->
    lager:notice("Hive Router is killing remaining clients: ~p", [Reason]),
    ets:foldl(fun({Pid, _Sid}, ok) when is_pid(Pid) ->
                      %% ...just die already...
                      hive_client:kill(Pid, Reason),
                      ok;

                 (_Otherwise, ok) ->
                      ok
              end,
              ok,
              State#state.clients),
    %% NOTE We're not really terminating, since Hive will imediately restart the Router.
    %% NOTE Instead, we'll patiently wait for Hive to be terminated by a SIGKILL or whatever.
    {noreply, State};

handle_info(Info, State) ->
    inc(?ROUTER_ERRORS),
    lager:warning("Unhandled Hive Router info message: ~p", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    inc(?ROUTER_ERRORS),
    lager:warning("Unhandled Hive Router code change."),
    {ok, State}.

%% Internal functions:
sha_hex(Data) ->
    list_to_binary(lists:flatten([io_lib:format("~.16b", [N]) || <<N>> <= crypto:hash(sha, Data)])).

get_client(Sid, Clients) ->
    case ets:lookup(Clients, Sid) of
        [{Sid, Pid}] -> Pid;
        []           -> unknown
    end.

spawn_client(State, Args) ->
    case State#state.ready of
        true ->
            Sid = sha_hex(term_to_binary(erlang:now())),
            lager:info("Spawning a new Client: ~s", [Sid]),
            Supervisor = State#state.supervisor,
            Clients = State#state.clients,
            case supervisor:start_child(Supervisor, [Sid | Args]) of
                {ok, Pid}      -> erlang:monitor(process, Pid),
                                  ets:insert(Clients, {Sid, Pid}),
                                  ets:insert(Clients, {Pid, Sid}), %% So we can remove clients by Pid without expensive ets:matches.
                                  inc(?TOTAL_SPAWNS),
                                  inc(?ROUTER_CLIENT_NUM),
                                  {ok, Sid};
                {error, Error} -> ErrorMsg = hive_error_utils:format("Unable to spawn new client process: ~p",
                                                                      [Error]),
                                  lager:warning(ErrorMsg),
                                  {error, {router_error, ErrorMsg}}
            end;
        false ->
            ErrorMsg = <<"Unable to spawn new client process: Hive Router is not accepting connections!">>,
            {error, {router_error, ErrorMsg}}
    end.

remove_client(Pid, Clients, Reason) ->
    case ets:lookup(Clients, Pid) of
        [{Pid, Sid}] ->
            lager:info("Removing terminated client ~s, reason: ~p", [Sid, Reason]),
            ets:delete(Clients, Pid),
            ets:delete(Clients, Sid),
            dec(?ROUTER_CLIENT_NUM);

        [] ->
            inc(?ROUTER_ERRORS),
            lager:warning("Tried removing an unknown Pid: ~p")
    end.

get_len(Stuff) when is_list(Stuff) ->
    length(Stuff);

get_len(_Stuff) ->
    1.
