-module(hive_web_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(WEB_SPECS(Mod), {Mod,
                         {hive_server_wrapper, start_link, [Mod]},
                         permanent,
                         5000,
                         worker,
                         [Mod]}).

%% External functions:
start_link() ->
    lager:notice("Starting Hive Server Supervisor..."),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {error, {shutdown, {failed_to_start_child, _Module, Error}}} ->
            lager:debug("Hive Server Supervisor encountered an error: ~p", [Error]),
            {error, Error};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Server Supervisor: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        Ret ->
            lager:notice("Hive Server Supervisor started!"),
            Ret
    end.

init([]) ->
    Strategy = {one_for_one, 5, 10},
    Processes = [?WEB_SPECS(hive_monitor_server),
                 ?WEB_SPECS(hive_api),
                 ?WEB_SPECS(hive_web)],
    {ok, {Strategy, Processes}}.
