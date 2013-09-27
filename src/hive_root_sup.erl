-module(hive_root_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(MODULE_SPECS(Mod), {Mod,
                            {Mod, start_link, []},
                            permanent,
                            %% NOTE Since the only children of this process are other
                            %% NOTE supervisors we're good with infinite tear-down time.
                            infinity,
                            supervisor,
                            [Mod]}).

start_link() ->
    lager:notice("Starting Hive Top-level Supervisor..."),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {error, {shutdown, {failed_to_start_child, _Module, Error}}} ->
            lager:debug("Hive Top-level Supervisor encountered an error: ~p", [Error]),
            {error, Error};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Top-level Supervisor: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        Ret ->
            lager:notice("Hive Top-level Supervisor started!"),
            Ret
    end.

init([]) ->
    Strategy = {one_for_one, 5, 10},
    Processes = [?MODULE_SPECS(hive_env_sup),
                 ?MODULE_SPECS(hive_sup)],
    {ok, {Strategy, Processes}}.

