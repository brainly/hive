-module(hive_env_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(MODULE_SPECS(Mod), {Mod,
                            {Mod, start_link, []},
                            permanent,
                            5000,
                            worker,
                            [Mod]}).

start_link() ->
    lager:notice("Starting Hive Environment Supervisor..."),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {error, {shutdown, {failed_to_start_child, _Module, Error}}} ->
            lager:debug("Hive Environment Supervisor encountered an error: ~p", [Error]),
            {error, Error};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Environment Supervisor: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        Ret ->
            lager:notice("Hive Environment Supervisor started!"),
            Ret
    end.

init([]) ->
    Strategy = {one_for_one, 5, 10},
    Processes = [?MODULE_SPECS(hive_monitor),
                 ?MODULE_SPECS(hive_plugins),
                 ?MODULE_SPECS(hive_config)],
    {ok, {Strategy, Processes}}.

