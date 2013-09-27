-module(hive_hooks).
-author('kajetan.rzepecki@zadane.pl').

-export([run/3, init/0, init/1, add/2, remove/2]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-include("hive_events.hrl").

%% External functions:
init() ->
    init(hive_config:get(<<"clients.hooks">>)).

init(Hooks) ->
    case hive_hooks_utils:init_hooks(Hooks) of
        {error, Error} -> inc(?HOOK_ERRORS),
                          {error, Error};
        Other          -> Other
    end.

run(Event, Trigger, State) ->
    %% NOTE run doesn't need to increment error counters, since the hooks themselves do this.
    hive_hooks_utils:run_hooks(Event, Trigger, State).

add(Hooks, State) ->
    case hive_hooks_utils:add_hooks(Hooks, State) of
        {error, Error} -> inc(?HOOK_ERRORS),
                          {error, Error};
        Other          -> Other
    end.

remove(Hooks, State) ->
    case hive_hooks_utils:remove_hooks(Hooks, State) of
        {error, Error} -> inc(?HOOK_ERRORS),
                          {error, Error};
        Other          -> Other
    end.
