-module(console_dump_hook).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(hive_plugin).

-export([load/0, unload/1, validate/2]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, inc/2]).

load() ->
    {ok, [{<<"utils.console_dump">>, fun init/1}], undefined}.

unload(_State) ->
    ok.

validate(<<"utils.console_dump">>, _Hook) ->
    ok.

init(_Args) ->
    {ok, fun console_dump/4}.

console_dump(Args, Event, Trigger, State) ->
    case hive_hooks_utils:state_json(Trigger, State) of
        {ok, JSON, NewState} ->
            lager:info("~s ~s", [Args, JSON]),
            {noreply, NewState};

        {error, Error, NewState} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Console Dump Hook encountered an error: ~p", [Error]),
            {error, Error, NewState};

        {stop, Reason, NewState} ->
            inc(?HOOK_ERRORS),
            inc(?HOOK_EVENT_ERRORS, Event),
            lager:debug("Hive Console Dump Hook encountered an error: ~p", [Reason]),
            {stop, Reason, NewState}
    end.
