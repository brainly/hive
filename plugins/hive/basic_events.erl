-module(basic_events).
-author('kajetan.rzepecki@zadane.pl').

-export([load/0, unload/1, validate/2]).

-include("hive_socketio.hrl").
-include("hive_events.hrl").

-import(hive_client_state_manager, [set_value/3]).

%% External functions:
load() ->
    {ok, [{<<"action.stop">>, fun init_stop/1},
          {<<"action.error">>, fun init_error/1},
          {<<"action.send_event">>, fun init_event/1},
          {<<"action.send_message">>, fun init_message/1},
          {<<"action.send_json">>, fun init_json/1},
          {<<"action.update_state">>, fun init_store/1}], undefined}.

unload(_State) ->
    ok.

validate(_Name, _Event) ->
    ok.

%% Internal functions:
init_stop(_Args) ->
    {ok, fun stop/4}.

stop(_Args, _Action, Trigger, State) ->
    %% NOTE Since we _requested_ a shutdown it should happen gracefully.
    {stop, {shutdown, {stopped, Trigger#internal_event.args}}, State}.

init_error(_Args) ->
    {ok, fun error/4}.

error(_Args, _Action, Trigger, State) ->
    {error, {internal_error, Trigger#internal_event.args}, State}.

init_event(_Args) ->
    {ok, fun event/4}.

event(_Args, _Action, Trigger, State) ->
    Event = to_json(Trigger#internal_event.args),
    case hive_config:validate(<<"external_event">>, Event) of
        {ok, _}        -> {reply, #sio_message{type = event, data = Trigger#internal_event.args}, State};
        {error, Error} -> {error, Error, State}
    end.

init_message(_Args) ->
    {ok, fun message/4}.

message(_Args, _Action, Trigger, State) ->
    {reply, #sio_message{type = message, data = Trigger#internal_event.args}, State}.

init_json(_Args) ->
    {ok, fun json/4}.

json(_Args, _Action, Trigger, State) ->
    {reply, #sio_message{type = json, data = Trigger#internal_event.args}, State}.

init_store(_Args) ->
    {ok, fun store/4}.

store(_Args, _Action, Trigger, State) ->
    %% FIXME Validate Data here.
    case set_value(all, to_json(Trigger#internal_event.args), State) of
        {ok, _Value, NewState}   -> {noreply, NewState};
        {error, Error, NewState} -> {error, Error, NewState};
        {stop, Reason, NewState} -> {stop, Reason, NewState}
    end.

%% Internal functions:
to_json(Thing) ->
    jsonx:decode(Thing, [{format, proplist}]).
