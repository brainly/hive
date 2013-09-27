-module(tcp_worker_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(WORKER_SPEC(Mod), {Mod,
                           {Mod, start_link, []},
                           temporary,
                           5000,
                           worker,
                           [Mod]}).

%% Supervisor callbacks
start_link() ->
    case supervisor:start_link(?MODULE, []) of
        {error, Error} -> inc(?HIVE_ERRORS),
                          ErrorMsg = hive_error_utils:format("Cannot start Hive TCP Connectors worker supervisor: ~p",
                                                              [Error]),
                          lager:error(ErrorMsg),
                          {error, {tcp_error, ErrorMsg}};
        Ret            -> Ret
    end.

init([]) ->
    Strategy = {simple_one_for_one, 5, 10},
    Processes = [?WORKER_SPEC(tcp_worker)],
    {ok, {Strategy, Processes}}.

