-module(dispatch_hook).
-author('kajetan.rzepecki@zadane.pl').

-export([load/0, unload/1, validate/2]).

-include("hive_socketio.hrl").
-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, inc/2]).

load() ->
    {ok, [{<<"utils.dispatch">>, fun init/1}], undefined}.

unload(_State) ->
    ok.

validate(<<"utils.dispatch">>, _Hook) ->
    ok.

init(_Args) ->
    {ok, fun dispatch/4}.

dispatch(_Args, Event, Trigger, State) ->
    ExternalEvent = jsonx:decode(Trigger#sio_message.data, [{format, proplist}]),
    Events = proplists:get_value(<<"args">>, ExternalEvent),
    case catch make_events(Events) of
        {error, Error}      -> inc(?HOOK_ERRORS),
                               inc(?HOOK_EVENT_ERRORS, Event),
                               lager:debug("Hive Dispatch Hook encountered an error: ~p", [Error]),
                               {error, Error, State};
        InternalEvents     -> hive_events:dispatch(InternalEvents, State)
    end.

make_events([]) ->
    [];

make_events([Event | Events]) ->
    case hive_events:new(Event) of
        {ok, Evnt}     -> [Evnt | make_events(Events)];
        {error, Error} -> throw({error, Error})
    end.
