-module(connectors_events).
-author('kajetan.rzepecki@zadane.pl').

-export([load/0, unload/1, validate/2]).

-include("hive_events.hrl").

%% External functions:
load() ->
    {ok, [{<<"action.start_connectors">>, fun init_start_connectors/1},
          {<<"action.stop_connectors">>, fun init_stop_connectors/1}], undefined}.

unload(_State) ->
    ok.

validate(_Name, _Event) ->
    ok.

%% Internal functions:
init_start_connectors(_Args) ->
    {ok, fun start_connectors/4}.

start_connectors(_Args, _Action, Trigger, State) ->
    Connectors = to_json(Trigger#internal_event.args),
    case hive_connectors:start_connectors(Connectors) of
        ok             -> {noreply, State};
        {error, Error} -> {error, Error, State}
    end.

init_stop_connectors(_Args) ->
    {ok, fun stop_connectors/4}.

stop_connectors(_Args, _Action, Trigger, State) ->
    Connectors = to_json(Trigger#internal_event.args),
    case hive_connectors:stop_connectors(Connectors) of
        ok             -> {noreply, State};
        {error, Error} -> {error, Error, State}
    end.

%% Internal functions:
to_json(Thing) ->
    jsonx:decode(Thing, [{format, proplist}]).
