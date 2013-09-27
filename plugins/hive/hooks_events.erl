-module(hooks_events).
-author('kajetan.rzepecki@zadane.pl').

-export([load/0, unload/1, validate/2]).

-include("hive_socketio.hrl").
-include("hive_events.hrl").

%% External functions:
load() ->
    {ok, [{<<"action.dispatch">>, fun init_dispatch/1},
          {<<"action.add_hooks">>, fun init_add_hooks/1},
          {<<"action.remove_hooks">>, fun init_remove_hooks/1}], undefined}.

unload(_State) ->
    ok.

validate(_Name, _Event) ->
    ok.

%% Internal functions:
init_dispatch(_Args) ->
    {ok, fun dispatch/4}.

dispatch(_Args, _Action, Trigger, State) ->
    Event = to_json(Trigger#internal_event.args),
    case hive_config:validate(<<"external_event">>, Event) of
        {ok, _}        -> EventName = proplists:get_value(<<"name">>, Event),
                          hive_hooks:run(EventName, #sio_message{type = event, data = Trigger#internal_event.args}, State);
        {error, Error} -> {error, Error, State}
    end.

init_add_hooks(_Args) ->
    {ok, fun add_hooks/4}.

add_hooks(_Args, _Action, Trigger, State) ->
    Hooks = to_json(Trigger#internal_event.args),
    case hive_hooks:add(Hooks, State) of
        {ok, NewState} -> {noreply, NewState};
        {error, Error} -> {error, Error, State}
    end.

init_remove_hooks(_Args) ->
    {ok, fun remove_hooks/4}.

remove_hooks(_Args, _Action, Trigger, State) ->
    Hooks = to_json(Trigger#internal_event.args),
    case hive_hooks:remove(Hooks, State) of
        {ok, NewState} -> {noreply, NewState};
        {error, Error} -> {error, Error, State}
    end.

to_json(Thing) ->
    jsonx:decode(Thing, [{format, proplist}]).
