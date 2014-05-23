-module(hive_plugins_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

%% External functions:
start_link() ->
    lager:notice("Starting Hive Plugins Supervisor..."),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {error, {shutdown, {failed_to_start_child, _Module, Error}}} ->
            lager:debug("Hive Plugins Supervisor encountered an error: ~p", [Error]),
            {error, Error};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Plugins Supervisor Supervisor: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        Ret ->
            lager:notice("Hive Plugins Supervisor started!"),
            Ret
    end.

init([]) ->
    Strategy = {one_for_all, 5, 10},
    Processes = [{hive_plugins,
                  {hive_plugins, start_link, []},
                  permanent,
                  5000,
                  worker,
                  [hive_plugins]}],
    {ok, {Strategy, Processes}}.
