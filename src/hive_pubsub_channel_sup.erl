-module(hive_pubsub_channel_sup).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(CHANNEL_SPEC(Mod), {Mod,
                            {Mod, start_link, []},
                            temporary,
                            5000,
                            worker,
                            [Mod]}).

%% External functions:
start_link() ->
    lager:notice("Starting Hive Pub-Sub Channel Supervisor..."),
    case supervisor:start_link(?MODULE, []) of
        {error, {shutdown, {failed_to_start_child, _Module, Error}}} ->
            lager:debug("Hive Pub-Sub Channel Supervisor encountered an error: ~p", [Error]),
            {error, Error};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Pub-Sub Channel Supervisor: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        Ret ->
            lager:notice("Hive Pub-Sub Channel Supervisor started!"),
            Ret
    end.

init([]) ->
    Strategy = {simple_one_for_one, 10, 10},
    Pools = [?CHANNEL_SPEC(hive_pubsub_channel)],
    {ok, {Strategy, Pools}}.

