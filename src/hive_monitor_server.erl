-module(hive_monitor_server).
-author('kajetan.rzepecki@zadane.pl').

-export([start/0, stop/0]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

start() ->
    lager:notice("Starting Hive Monitor Server..."),
    %% Hive monitor RESTful API:
    Dispatch = cowboy_router:compile([{'_', [{"/monitor[/]", hive_monitor_http_handler, []},
                                             {"/monitor/:hash[/]", hive_monitor_http_handler, []},
                                             {"/monitor/:hash/:prefix[/]", hive_monitor_http_handler, []}]}]),

    case cowboy:start_http(http, hive_config:get(<<"monitor.acceptors">>),
                           [{port, hive_config:get(<<"monitor.port">>)},
                            {max_connections, 1024}],
                           [{env, [{dispatch, Dispatch}]}])
    of
        {error, {already_started, Pid}} ->
            inc(?HIVE_ERRORS),
            lager:warning("Hive Monitor Server is already started!"),
            erlang:link(Pid),
            {ok, Pid};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Monitor Server: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        {ok, Pid} ->
            lager:notice("Hive Monitor Server started!"),
            erlang:link(Pid),
            {ok, Pid}
    end.

stop() ->
    ok.
