-module(pubsub_events).
-author('kajetan.rzepecki@brainly.com').

-export([load/0, unload/1, validate/2]).

-include("hive_events.hrl").

-import(hive_pubsub_utils, [make_cids/1]).

%% External functions:
load() ->
    {ok, [{<<"action.pubsub.publish">>, fun init_publish/1},
          {<<"action.pubsub.publish_raw">>, fun init_publish_raw/1},
          {<<"action.pubsub.subscribe">>, fun init_subscribe/1},
          {<<"action.pubsub.unsubscribe">>, fun init_unsubscribe/1},
          {<<"action.pubsub.resubscribe">>, fun init_resubscribe/1}], undefined}.

unload(_State) ->
    ok.

validate(_Name, _Event) ->
    ok.

%% Internal functions:
init_publish(_Args) ->
    {ok, fun publish/4}.

publish(Privilege, _Action, Trigger, State) ->
    Args = jsonx:decode(Trigger#internal_event.args, [{format, proplist}]),
    EventToSend = proplists:get_value(<<"event">>, Args),
    Cids = make_cids(proplists:get_value(<<"cids">>, Args, [])),
    case hive_events:new(EventToSend) of
        {ok, Evnt} ->
            case hive_pubsub:publish(Privilege, Cids, Evnt) of
                ok             -> {noreply, State};
                {error, Error} -> lager:debug("Hive Pub-Sub event dispatcher encountered an error: ~p", [Error]),
                                  {error, Error, State}
            end;
        {error, Error} ->
            lager:debug("Hive Pub-Sub event dispatcher encountered an error: ~p", [Error]),
            {error, Error, State}
    end.

init_publish_raw(_Args) ->
    {ok, fun publish_raw/4}.

publish_raw(Privilege, _Action, Trigger, State) ->
    Args = jsonx:decode(Trigger#internal_event.args, [{format, proplist}]),
    EventToSend = proplists:get_value(<<"event">>, Args),
    Cids = make_cids(proplists:get_value(<<"cids">>, Args, [])),
    case hive_pubsub:publish(Privilege, Cids, jsonx:encode(EventToSend)) of
        ok             -> {noreply, State};
        {error, Error} -> lager:debug("Hive Pub-Sub event dispatcher encountered an error: ~p", [Error]),
                          {error, Error, State}
    end.

init_subscribe(_Args) ->
    {ok, fun subscribe/4}.

subscribe(Privilege, _Action, Trigger, State) ->
    Cids = make_cids(jsonx:decode(Trigger#internal_event.args, [{format, proplist}])),
    case hive_pubsub:join(Privilege, Cids) of
        ok             -> {noreply, State};
        {error, Error} -> lager:debug("Hive Pub-Sub event dispatcher encountered an error: ~p", [Error]),
                          {error, Error, State}
    end.

init_unsubscribe(_Args) ->
    {ok, fun unsubscribe/4}.

unsubscribe(Privilege, _Action, Trigger, State) ->
    Cids = make_cids(jsonx:decode(Trigger#internal_event.args, [{format, proplist}])),
    case hive_pubsub:leave(Privilege, Cids) of
        ok             -> {noreply, State};
        {error, Error} -> lager:debug("Hive Pub-Sub event dispatcher encountered an error: ~p", [Error]),
                          {error, Error, State}
    end.

init_resubscribe(_Args) ->
    {ok, fun resubscribe/4}.

resubscribe(Privilege, _Action, Trigger, State) ->
    Cids = jsonx:decode(Trigger#internal_event.args, [{format, proplist}]),
    DroppedCids = lists:map(fun ({Prefix, _Rest}) -> Prefix end, Cids),
    NewCids = make_cids(Cids),
    case hive_pubsub:leave(Privilege, DroppedCids) of
        ok ->
            case hive_pubsub:join(Privilege, NewCids) of
                ok             -> {noreply, State};
                {error, Error} -> lager:debug("Hive Pub-Sub event dispatcher encountered an error: ~p", [Error]),
                                  {error, Error, State}
            end;

        {error, Error} ->
            lager:debug("Hive Pub-Sub event dispatcher encountered an error: ~p", [Error]),
            {error, Error, State}
    end.
