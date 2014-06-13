-module(pubsub_hook).
-author('kajetan.rzepecki@zadane.pl').

-export([load/0, unload/1, validate/2]).

-include("hive_socketio.hrl").
-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, inc/2]).

load() ->
    {ok, [{<<"pubsub.publish">>, fun init_publish/1},
          {<<"pubsub.subscribe">>, fun init_subscribe/1},
          {<<"pubsub.unsubscribe">>, fun init_unsubscribe/1},
          {<<"pubsub.resubscribe">>, fun init_resubscribe/1}], undefined}.

unload(_State) ->
    ok.

validate(_Name, _Hook) ->
    ok.

init_publish(Args) ->
    case proplists:get_value(<<"raw">>, Args, false) of
        true  -> {ok, fun publish_raw/4};
        false -> {ok, fun publish/4}
    end.

publish(Args, Event, Trigger, State) ->
    ExternalEvent = jsonx:decode(Trigger#sio_message.data, [{format, proplist}]),
    [EventToSend] = proplists:get_value(<<"args">>, ExternalEvent, []),
    Privilege = proplists:get_value(<<"privilege">>, Args),
    Cids = proplists:get_value(<<"cids">>, Args),
    inc(?HOOK_PUBSUB_PUB),
    inc(?HOOK_EVENT_PUBSUB_PUB, Event),
    case hive_events:new(EventToSend) of
        {ok, Evnt} ->
            case hive_pubsub:publish(Privilege, make_cids(Cids), Evnt) of
                ok             -> {noreply, State};
                {error, Error} -> inc(?HOOK_ERRORS),
                                  inc(?HOOK_EVENT_ERRORS, Event),
                                  lager:debug("Hive Pub-Sub Hook encountered an error: ~p", [Error]),
                                  {error, Error, State}
            end;

        {error, Error} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Pub-Sub Hook encountered an error: ~p", [Error]),
            {error, Error, State}
    end.

publish_raw(Args, Event, Trigger, State) ->
    ExternalEvent = jsonx:decode(Trigger#sio_message.data, [{format, proplist}]),
    [EventToSend] = proplists:get_value(<<"args">>, ExternalEvent, []),
    Privilege = proplists:get_value(<<"privilege">>, Args),
    Cids = proplists:get_value(<<"cids">>, Args),
    inc(?HOOK_PUBSUB_PUB),
    inc(?HOOK_EVENT_PUBSUB_PUB, Event),
    case hive_pubsub:publish(Privilege, make_cids(Cids), jsonx:encode(EventToSend)) of
        ok             -> {noreply, State};
        {error, Error} -> inc(?HOOK_ERRORS),
                          inc(?HOOK_EVENT_ERRORS, Event),
                          lager:debug("Hive Pub-Sub Hook encountered an error: ~p", [Error]),
                          {error, Error, State}
    end.

init_subscribe(_Args) ->
    {ok, fun subscribe/4}.

subscribe(Privilege, Event, Trigger, State) ->
    ExternalEvent = jsonx:decode(Trigger#sio_message.data, [{format, proplist}]),
    [Cids] = proplists:get_value(<<"args">>, ExternalEvent, []),
    inc(?HOOK_PUBSUB_SUB),
    inc(?HOOK_EVENT_PUBSUB_SUB, Event),
    case hive_pubsub:join(Privilege, make_cids(Cids)) of
        ok             -> {noreply, State};
        {error, Error} -> inc(?HOOK_ERRORS),
                          inc(?HOOK_EVENT_ERRORS, Event),
                          lager:debug("Hive Pub-Sub Hook encountered an error: ~p", [Error]),
                          {error, Error, State}
    end.

init_unsubscribe(_Args) ->
    {ok, fun unsubscribe/4}.

unsubscribe(Privilege, Event, Trigger, State) ->
    ExternalEvent = jsonx:decode(Trigger#sio_message.data, [{format, proplist}]),
    [Cids] = proplists:get_value(<<"args">>, ExternalEvent, []),
    inc(?HOOK_PUBSUB_UNSUB),
    inc(?HOOK_EVENT_PUBSUB_UNSUB, Event),
    case hive_pubsub:leave(Privilege, make_cids(Cids)) of
        ok             -> {noreply, State};
        {error, Error} -> inc(?HOOK_ERRORS),
                          inc(?HOOK_EVENT_ERRORS, Event),
                          lager:debug("Hive Pub-Sub Hook encountered an error: ~p", [Error]),
                          {error, Error, State}
    end.

init_resubscribe(_Args) ->
    {ok, fun resubscribe/4}.

resubscribe(Privilege, Event, Trigger, State) ->
    ExternalEvent = jsonx:decode(Trigger#sio_message.data, [{format, proplist}]),
    [CidDescr] = proplists:get_value(<<"args">>, ExternalEvent, []),
    DroppedCids = lists:map(fun ({Prefix, _Rest}) -> Prefix end, CidDescr),
    NewCids = make_cids(CidDescr),
    inc(?HOOK_PUBSUB_UNSUB),
    inc(?HOOK_EVENT_PUBSUB_UNSUB, Event),
    case hive_pubsub:leave(Privilege, DroppedCids) of
        ok ->
            case hive_pubsub:join(Privilege, NewCids) of
                ok             -> {noreply, State};
                {error, Error} -> inc(?HOOK_ERRORS),
                                  inc(?HOOK_EVENT_ERRORS, Event),
                                  lager:debug("Hive Pub-Sub Hook encountered an error: ~p", [Error]),
                                  {error, Error, State}
            end;

        {error, Error} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Pub-Sub Hook encountered an error: ~p", [Error]),
            {error, Error, State}
    end.

%% Internal functions:
make_cids(Cid) when is_binary(Cid) ->
    [Cid];

make_cids(Cid) when is_integer(Cid) ->
    [integer_to_binary(Cid)];

make_cids([]) ->
    [];

make_cids([Prefix | Rest]) when is_binary(Prefix) ->
    lists:flatten([Prefix | make_cids(Rest)]);

make_cids([Prefix | Rest]) when is_binary(Prefix) ->
    lists:flatten([integer_to_binary(Prefix) | make_cids(Rest)]);

make_cids([{Prefix, Ids} | Rest]) ->
    lists:flatten([make_cids(Prefix, Ids) | make_cids(Rest)]);

make_cids([Cid | Rest]) when is_binary(Cid) ->
    lists:flatten([Cid | make_cids(Rest)]).

make_cids(_Prefix, []) ->
    [];

make_cids(Prefix, Types = [{_Type, _Ids} | _]) ->
    SubjectIds = proplists:get_value(<<"subjects">>, Types, []),
    SubjectIds2 = [integer_to_binary(Id) || Id <- SubjectIds],

    GradeIds   = proplists:get_value(<<"grades">>, Types, []),
    GradeIds2  = [integer_to_binary(Id) || Id <- GradeIds],

    lager:debug("SIDS: ~p, GIDS: ~p", [SubjectIds2, GradeIds2]),

    lists:map(fun(SubId) ->
                      Part1 = <<Prefix/binary, ".", SubId/binary>>,
                      [<<Part1/binary, ".", GradeId/binary>> || GradeId <- GradeIds2]
              end,
              SubjectIds2);

make_cids(Prefix, [Id | Ids]) ->
    lists:flatten([make_cids(Prefix, Id) | make_cids(Prefix, Ids)]);

make_cids(Prefix, Id) when is_integer(Id) ->
    IdBin = integer_to_binary(Id),
    [<<Prefix/binary, ".", IdBin/binary>>];

make_cids(Prefix, Id) when is_binary(Id) ->
    [<<Prefix/binary, ".", Id/binary>>].

