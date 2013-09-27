-module(hive_router_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(ROUTER_SPECS(Mod, Sup), {Mod,
                                 {Mod, start_link, [Sup]},
                                 permanent,
                                 5000,
                                 worker,
                                 [Mod]}).

%% External functions:
start_link() ->
    lager:notice("Starting Hive Router Supervisor..."),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {error, {shutdown, {failed_to_start_child, _Module, Error}}} ->
            lager:debug("Hive Router Supervisor encountered an error: ~p", [Error]),
            {error, Error};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Router Supervisor Supervisor: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        Ret ->
            lager:notice("Hive Router Supervisor started!"),
            Ret
    end.

init([]) ->
    Strategy = {one_for_all, 5, 10},
    Processes = [?ROUTER_SPECS(hive_router, self())],
    {ok, {Strategy, Processes}}.
