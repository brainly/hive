-module(hive_connectors_pool_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(CONNECTOR_POOL_SPECS(Mod), {Mod,
                                    {Mod, start_connector, []},
                                    transient, %% NOTE Pools are only restarted when they exit abnormally.
                                    5000,
                                    worker,
                                    dynamic}).

%% External functions:
start_link() ->
    lager:notice("Starting Hive Connectors Pool Supervisor..."),
    case supervisor:start_link(?MODULE, []) of
        {error, {shutdown, {failed_to_start_child, _Module, Error}}} ->
            lager:debug("Hive Connectors Pool Supervisor encountered an error: ~p", [Error]),
            {error, Error};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Connectors Pool Supervisor: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        Ret ->
            lager:notice("Hive Connectors Pool Supervisor started!"),
            Ret
    end.

init([]) ->
    Strategy = {simple_one_for_one, 10, 10},
    Pools = [?CONNECTOR_POOL_SPECS(hive_connectors)],
    {ok, {Strategy, Pools}}.

