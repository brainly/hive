-module(hive_client_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(CLIENT_SPEC(Mod), {Mod,
                           {Mod, start_link, []},
                           temporary,
                           5000,
                           worker,
                           [Mod]}).

%% Supervisor callbacks
start_link() ->
    lager:notice("Starting Hive Clients Supervisor..."),
    case supervisor:start_link(?MODULE, []) of
        {error, {shutdown, {failed_to_start_child, _Module, Error}}} ->
            lager:debug("Hive Clients Supervisor encountered an error: ~p", [Error]),
            {error, Error};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Clients Supervisor: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        Ret ->
            lager:notice("Hive Clients Supervisor started!"),
            Ret
    end.

init([]) ->
    Strategy = {simple_one_for_one, 5, 10},
    Processes = [?CLIENT_SPEC(hive_client)],
    {ok, {Strategy, Processes}}.
