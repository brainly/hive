-module(hive_api).
-author('kajetan.rzepecki@zadane.pl').

-export([start/0, stop/0]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

start() ->
    lager:notice("Starting Hive API Server..."),
    Dispatch = cowboy_router:compile([{'_', [%% Hive related API:
                                             {"/api/:apihash/hive/:action[/]", hive_api_handler, []},
                                             %% Router related API:
                                             {"/api/:apihash/router/:action[/]", hive_router_api_handler, []},
                                             %% Client related API:
                                             {"/api/:apihash/clients/:action/:sid[/]", hive_client_api_handler, []},
                                             %% Pub-Sub related API:
                                             {"/api/:apihash/pubsub/:action/:id[/]", hive_pubsub_api_handler, []}]}]),

    case cowboy:start_http(api, hive_config:get(<<"api.acceptors">>),
                           [{port, hive_config:get(<<"api.port">>)},
                            {max_connections, 1024}],
                           [{env, [{dispatch, Dispatch}]}])
    of
        {error, {already_started, Pid}} ->
            inc(?HIVE_ERRORS),
            lager:warning("Hive API Server is already started!"),
            erlang:link(Pid),
            {ok, Pid};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive API Server: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        {ok, Pid} ->
            lager:notice("Hive API Server started!"),
            erlang:link(Pid),
            {ok, Pid}
    end.

stop() ->
    ok.
