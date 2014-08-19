-module(hive_pubsub).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/1, init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([publish/2, publish/3, subscribe/2, subscribe/3, unsubscribe/2, unsubscribe/3]).
-export([join/1, join/2, join/3, leave/1, leave/2, leave/3, status/1, uptime/0]).

-include("hive_events.hrl").
-include("hive_monitor.hrl").
-import(hive_monitor_utils, [init_counters/1, name/2, inc/1, dec/1, incby/2]).

-record(state, {supervisor, pub_channels, sub_channels, start_time}).

-define(CHANNEL_SUP_SPECS(Mod), {Mod,
                                 {Mod, start_link, []},
                                 permanent,
                                 infinity,
                                 supervisor,
                                 [Mod]}).

-define(PRIVILEGE_LEVELS, [{<<"public">>, 0}, {<<"private">>, 1}]).
-define(MAX_PRIVILEGE, 1).

%% Gen Server related:
start_link(PoolSup) ->
    lager:notice("Starting Hive Pub-Sub..."),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, PoolSup, [{spawn_opt, [{fullsweep_after, 10}]}]) of
        {error, Error} -> lager:debug("Hive Pub-Sub encountered an error: ~p.", [Error]),
                          {error, Error};
        Ret            -> lager:notice("Hive Pub-Sub started!"),
                          Ret
    end.

init(PoolSup) ->
    lists:foreach(fun({Prefix, _Value}) ->
                          add_counters(Prefix)
                  end,
                  hive_config:get(<<"pubsub.channels">>)),
    gen_server:cast(?MODULE, {start_channel_sup, PoolSup}),
    StartTime = hive:uptime(),
    {ok, #state{pub_channels = ets:new(hive_pubsub_pub, [bag]),
                sub_channels = ets:new(hive_pubsub_sub, []),
                start_time = StartTime}}.

terminate(Reason, State) ->
    lager:notice("Hive Pub-Sub terminated: ~n- State: ~p~n- Reason: ~p", [State, Reason]),
    ets:delete(State#state.pub_channels),
    ets:delete(State#state.sub_channels),
    ok.

%% External functions:
uptime() ->
    %% NOTE No need to increment PUBSUB_REQUESTS since this isn't really a request.
    gen_server:call(?MODULE, uptime).

status(Cid) ->
    inc(?PUBSUB_REQUESTS),
    inc(?PUBSUB_STATUS),
    gen_server:call(?MODULE, {status, Cid}).

publish(Cid, Events) ->
    publish(?MAX_PRIVILEGE, Cid, Events).

publish(Privilege, Cid, Events) ->
    inc(?PUBSUB_REQUESTS),
    inc(?PUBSUB_PUBLISH),
    hive_cluster:call(?MODULE, {publish, Privilege, Cid, Events}).

join(Cids) ->
    join(?MAX_PRIVILEGE, Cids).

join(Privilege, Cids) ->
    inc(?PUBSUB_REQUESTS),
    inc(?PUBSUB_JOIN),
    gen_server:call(?MODULE, {join, Privilege, self(), Cids}).

subscribe(Sid, Cids) ->
    subscribe(Sid, Cids, ?MAX_PRIVILEGE).

subscribe(Sid, Cids, Privilege) ->
    inc(?PUBSUB_REQUESTS),
    inc(?PUBSUB_SUBSCRIBE),
    %% NOTE This is used by the PubSub API to convert Sid into a Pid.
    trace(hive_router:route_event(Sid, {exec, {?MODULE, join, [Privilege, Cids]}})).

leave(Cids) ->
    leave(?MAX_PRIVILEGE, Cids).

leave(Privilege, Cids) ->
    inc(?PUBSUB_REQUESTS),
    inc(?PUBSUB_LEAVE),
    gen_server:call(?MODULE, {leave, Privilege, self(), Cids}).

unsubscribe(Sid, Cids) ->
    unsubscribe(Sid, Cids, ?MAX_PRIVILEGE).

unsubscribe(Sid, Cids, Privilege) ->
    inc(?PUBSUB_REQUESTS),
    inc(?PUBSUB_UNSUBSCRIBE),
    %% NOTE This is used by the PubSub API to convert Sid into a Pid.
    trace(hive_router:route_event(Sid, {exec, {?MODULE, leave, [Privilege, Cids]}})).

%% Gen Server handlers:
handle_call(uptime, _From, State) ->
    {reply, hive:uptime() - State#state.start_time, State};

handle_call({status, Cid}, _From, State) ->
    case get_channels(Cid, State#state.pub_channels) of
        [] ->
            Error = {unknown_channel_id, err_log("Tried accessing an unknown Hive Pub-Sub Channel: ~s",
                                                 [Cid])},
            {reply, {error, Error}, State};

        Channels ->
            Reply = lists:sum(lists:map(fun hive_pubsub_channel:status/1, Channels)),
            {reply, {ok, Reply}, State}
    end;

handle_call({join, Privilege, Pid, Cids}, _From, State) ->
    case check_privilege(Privilege, Cids, State) of
        ok ->
            {reply, channel_fold(fun(Cid) ->
                                         %% When there are no channels with a given Cid:
                                         case new_channel(Cid, State) of
                                             {error, Error} ->
                                                 {error, Error};

                                             {_Priv, Channel} ->
                                                 hive_pubsub_channel:subscribe(Channel, Pid)
                                         end
                                 end,
                                 fun({_Priv, Channel}) ->
                                         %% When there are channels with a given Cid:
                                         hive_pubsub_channel:subscribe(Channel, Pid)
                                 end,
                                 lists:zip(Cids, get_channels(Cids, State#state.sub_channels))),
             State};

        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call({leave, Privilege, Pid, Cids}, _From, State) ->
    case check_privilege(Privilege, Cids, State) of
        ok ->
            {reply, channel_fold(fun(Cid) ->
                                         %% When there are no channels with a given Cid there's nothing to do.
                                         dbg_log("Tried unsubscribing an unknown channel: ~s", [Cid]),
                                         ok
                                 end,
                                 fun(Channel) ->
                                         %% When there are some channels with a given Cid:
                                         hive_pubsub_channel:unsubscribe(Channel, Pid)
                                 end,
                                 lists:zip(Cids, get_channels(Cids, State#state.pub_channels))),
             State};

        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call({publish, Privilege, Cids, Events}, _From, State) ->
    incby(?PUBSUB_PUBLISHED, get_length(Events)),
    case check_privilege(Privilege, Cids, State) of
        ok ->
            {reply, channel_fold(fun(Cid) ->
                                         %% When there are no channels with a given Cid there's nothing to do.
                                         dbg_log("Tried publishing to an unknown channel: ~s", [Cid]),
                                         ok
                                 end,
                                 fun(Channel) ->
                                         %% When there are some channels with a given Cid:
                                         hive_pubsub_channel:publish(Channel, Events)
                                 end,
                                 lists:zip(Cids, get_channels(Cids, State#state.pub_channels))), State};

        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call(Action, _From, State) ->
    err_log("Unhandled Hive Pub-Sub call: ~p", [Action]),
    {noreply, State}.

handle_cast({start_channel_sup, PoolSup}, State) ->
    case supervisor:start_child(PoolSup, ?CHANNEL_SUP_SPECS(hive_pubsub_channel_sup)) of
        {ok, Pid} ->
            {noreply, State#state{supervisor = Pid}};

        {error, {already_started, Pid}}->
            inc(?PUBSUB_ERRORS),
            lager:warning("Hive Pub-Sub Channel Supervisor already started!"),
            {noreply, State#state{supervisor = Pid}};

        {error, Error} ->
            trace({error, Error}),
            {stop, Error, State}
    end;

handle_cast(Action, State) ->
    err_log("Unhandled Hive Pub-Sub cast: ~p", [Action]),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    remove_channels(Pid, State, Reason),
    {noreply, State};

handle_info(Info, State) ->
    err_log("Unhandled Hive Pub-Sub info message: ~p", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    err_log("Unhandled Hive Pub-Sub code change.", []),
    {ok, State}.

%% Internal functions:
get_length(Events) when is_list(Events) ->
    length(Events);

get_length(_) ->
    1.

get_channels(Cids, Channels) when is_list(Cids) ->
    lists:map(fun(Cid) -> get_channels(Cid, Channels) end, Cids);

get_channels(Cid, Channels) ->
    case ets:lookup(Channels, Cid) of
        [] -> [];
        Chans -> lists:map(fun({_Prefix, Channel}) -> Channel end, Chans)
    end.

new_channel(Cid, State) ->
    case spawn_channel(Cid, State) of
        {ok, {Privilege, Pid}} ->
            Prefixes = make_prefixes(Cid),
            %% NOTE We store concrete CID and privilege type for subscriptions...
            ets:insert(State#state.sub_channels, {Cid, {Privilege, Pid}}),
            %% NOTE ...and prefixes for publishing.
            ets:insert(State#state.pub_channels, {Pid, Prefixes}),
            lists:foreach(fun(Prefix) ->
                                  ets:insert(State#state.pub_channels, {Prefix, Pid})
                          end,
                          Prefixes),
            {Privilege, Pid};

        {error, Error}->
            {error, Error}
    end.

spawn_channel(Cid, State) ->
    Supervisor = State#state.supervisor,
    [Prefix | _Rest] = prefix(Cid),
    case channel_descr(Cid) of
        {error, Error} ->
            {error, Error};

        ChannelDescr ->
            Timeout = proplists:get_value(<<"timeout">>, ChannelDescr),
            Privilege = cid_privilege(ChannelDescr),
            inc(?PUBSUB_CHANNELS),
            case supervisor:start_child(Supervisor, [Prefix, Cid, Timeout]) of
                {ok, Pid} ->
                    erlang:monitor(process, Pid),
                    {ok, {Privilege, Pid}};

                {error, Error} ->
                    {error, {pubsub_error, err_log("Unable to spawn a new Hive Pub-Sub Channel process: ~p", [Error])}}
            end
    end.

make_prefixes(Cid) ->
    [Current | Rest] = prefix(Cid),
    make_prefixes(Current, Rest, []).

make_prefixes(Current, [], Prefixes) ->
    [Current | Prefixes];

make_prefixes(Current, [Next | Rest], Prefixes) ->
    NewCurrent = <<Current/binary, ".", Next/binary>>,
    make_prefixes(NewCurrent, Rest, [Current | Prefixes]).

remove_channels(Pid, State, Reason) ->
    case ets:lookup(State#state.pub_channels, Pid) of
        [{Pid, Prefixes = [Cid | _Rest]}] ->
            lager:info("Removing a terminated Hive Pub-Sub channel ~p, reason: ~p", [Pid, Reason]),
            %% NOTE In order to remove a channel, we first determine all of its prefixes
            %% NOTE and its original Cid (the first prefix), and remove all objects refering
            %% NOTE to the channel from both pub_channels and sub_channels.
            ets:delete(State#state.pub_channels, Pid),
            ets:delete(State#state.sub_channels, Cid),
            lists:foreach(fun(Prefix) ->
                                  ets:delete_object(State#state.pub_channels, {Prefix, Pid})
                          end,
                          Prefixes),
            dec(?PUBSUB_CHANNELS);

        [] ->
            err_log("Tried removing an unknown channel: ~p", [Pid])
    end.

prefix(Cid) ->
    binary:split(Cid, [<<".">>], [global]).

check_privilege(Privilege, Cids, State) when is_list(Cids) ->
    lists:foldl(fun(_Cid, {error, Reason}) ->
                        {error, Reason};

                   (Cid, ok) ->
                        check_privilege(Privilege, Cid, State)
                end,
                ok,
                Cids);

check_privilege(Privilege, Cid, State) ->
    check_privilege(Privilege, Cid, State, dont_fail).

check_privilege(?MAX_PRIVILEGE, _Cid, _State, _FailIfNoChannels) ->
    ok;

check_privilege(Privilege, Cid, State, FailIfNoChannels) ->
    case get_channels(Cid, State#state.sub_channels) of
        %% NOTE First call requires us to look into the descriptor...
        [] ->
            case FailIfNoChannels of
                fail_if_no_channels ->
                    {error, {unknown_channel_id, Cid}};

                _ ->
                    PrivLevel = privilege_level(Privilege),
                    case cid_privilege(channel_descr(Cid)) of
                        {error, Error} ->
                            {error, Error};

                        ChannelPriv when PrivLevel >= ChannelPriv ->
                            ok;

                        _ ->
                            Format = "Tried accessing a Hive Pub-Sub Channel with insufficient privilege level: ~s",
                            {error, {access_denied, err_log(Format, [Cid])}}
                    end

            end;

        %% NOTE ...but further calls can use the cached value.
        Channels ->
            case lists:all(fun({CPrivilege, _Channel}) ->
                                   privilege_level(Privilege) >= CPrivilege
                           end,
                           Channels)
            of
                true ->
                    ok;

                false ->
                    Format = "Tried accessing a Hive Pub-Sub Channel with insufficient privilege level: ~s",
                    {error, {access_denied, err_log(Format, [Cid])}}
            end
    end.

cid_privilege({error, Error}) ->
    {error, Error};

cid_privilege(ChannelDescr) ->
    Privilege = proplists:get_value(<<"privilege">>, ChannelDescr),
    privilege_level(Privilege).

privilege_level(Privilege) ->
    proplists:get_value(Privilege, ?PRIVILEGE_LEVELS).

channel_descr(Cid) ->
    [Prefix | _Rest] = prefix(Cid),
    case proplists:get_value(Prefix, hive_config:get(<<"pubsub.channels">>)) of
        undefined ->
            {error, {bad_channel_id, err_log("Requested Hive Pub-Sub Channel prefix is undefined: ~s",
                                             [Prefix])}};

        ChannelDescr ->
            ChannelDescr
    end.

-define(PREFIX_COUNTERS, [?PUBSUB_CHANNEL_REQUESTS, ?PUBSUB_CHANNEL_ERRORS, ?PUBSUB_CHANNEL_CHANNELS,
                          ?PUBSUB_CHANNEL_STATUS, ?PUBSUB_CHANNEL_SUBS, ?PUBSUB_CHANNEL_UNSUBS,
                          ?PUBSUB_CHANNEL_PUBLISH, ?PUBSUB_CHANNEL_SUBSCR, ?PUBSUB_CHANNEL_PUBLISHED]).

add_counters(Prefix) ->
    lists:foreach(fun(Counter) ->
                          init_counters(name(Counter, Prefix))
                  end,
                  ?PREFIX_COUNTERS).

reply(undefined, Reply) ->
    Reply;

reply(From, Reply) ->
    gen_server:reply(From, Reply),
    Reply.

error_fold(Fun, List) ->
    lists:foldl(fun (_, {error, Error}) -> {error, Error};
                    (Value, ok)         -> Fun(Value)
                end,
                ok,
                List).

channel_fold(OnEmpty, OnChannels, Channels) ->
    case error_fold(fun ({Cid, []})     -> OnEmpty(Cid);
                        ({_Cid, Chans}) -> error_fold(OnChannels, Chans)
                    end,
                    Channels)
    of
        ok->
            ok;

        {error, Error = {pubsub_channel_error, _Msg}} ->
            trace({error, Error});

        {error, Error} ->
            %% NOTE We don't want to log pubsub_errors twice.
            {error, Error}
    end.

trace({error, Error}) ->
    err_log("Hive Pub-Sub encountered an error: ~p", [Error]),
    {error, Error};

trace(Otherwise) ->
    Otherwise.

dbg_log(Format, Values) ->
    ErrorMsg = hive_error_utils:format(Format, Values),
    lager:debug(ErrorMsg),
    ErrorMsg.

err_log(Format, Values) ->
    inc(?PUBSUB_ERRORS),
    ErrorMsg = hive_error_utils:format(Format, Values),
    lager:warning(ErrorMsg),
    ErrorMsg.
