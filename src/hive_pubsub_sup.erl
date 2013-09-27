-module(hive_pubsub_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(PUBSUB_SPECS(Mod, Sup), {Mod,
                                 {Mod, start_link, [Sup]},
                                 permanent,
                                 5000,
                                 worker,
                                 [Mod]}).

%% External functions:
start_link() ->
    lager:notice("Starting Hive Pub-Sub Supervisor..."),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {error, {shutdown, {failed_to_start_child, _Module, Error}}} ->
            lager:debug("Hive Pub-Sub Supervisor encountered an error: ~p", [Error]),
            {error, Error};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Pub-Sub Supervisor: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        Ret ->
            lager:notice("Hive Pub-Sub Supervisor started!"),
            Ret
    end.

init([]) ->
    Strategy = {one_for_all, 5, 10},
    Processes = [?PUBSUB_SPECS(hive_pubsub, self())],
    {ok, {Strategy, Processes}}.

