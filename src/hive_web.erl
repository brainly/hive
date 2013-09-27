-module(hive_web).
-author('kajetan.rzepecki@zadane.pl').

-export([start/0, stop/0]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

start() ->
    Transports = hive_config:get(<<"socketio.transports">>),
    HiveConfig = [hive_config:get(<<"socketio.heartbeat_timeout">>) div 1000,
                   hive_config:get(<<"socketio.reconnect_timeout">>) div 1000,
                   Transports],
    StaticConfig = [{directory, <<"priv/static">>},
                    {mimetypes, [{<<".swf">>, [<<"application/x-shockwave-flash">>]},
                                 {<<".xml">>, [<<"text/xml">>]}]}],

    lager:notice("Starting Hive Server..."),
    Dispatch = cowboy_router:compile([{'_', [%% Main receiving loop:
                                             {"/socket.io/static/[...]", cowboy_static, StaticConfig},
                                             {"/socket.io/1/", hive_loop_handler, HiveConfig}
                                             %% Specific transport-accepting loops:
                                             | build_dispatcher(Transports)]}]),

    case cowboy:start_http(server, hive_config:get(<<"hive.acceptors">>),
                           [{port, hive_config:get(<<"hive.port">>)},
                            {max_connections, 1000000}], %% Go nuts
                           [{env, [{dispatch, Dispatch}]}])
    of
        {error, {already_started, Pid}} ->
            inc(?HIVE_ERRORS),
            lager:warning("Hive Server is already started!"),
            erlang:link(Pid),
            {ok, Pid};

        {error, Error} ->
            inc(?HIVE_ERRORS),
            ErrorMsg = hive_error_utils:format("Cannot start Hive Server: ~p", [Error]),
            lager:error(ErrorMsg),
            {error, {hive_error, ErrorMsg}};

        {ok, Pid} ->
            lager:notice("Hive Server started!"),
            erlang:link(Pid),
            {ok, Pid}
    end.

stop() ->
    ok.

build_dispatcher([]) ->
    [];

build_dispatcher([<<"websocket">> | Rest]) ->
    %% WebSocket transport handler:
    [{"/socket.io/1/websocket/:sid", hive_websocket_handler, []} | build_dispatcher(Rest)];

build_dispatcher([<<"flashsocket">> | Rest]) ->
    %% WebSocket transport handler:
    [{"/socket.io/1/flashsocket/:sid", hive_websocket_handler, []} | build_dispatcher(Rest)];

build_dispatcher([<<"xhr-polling">> | Rest]) ->
    %% XHR polling transport handler:
    [{"/socket.io/1/xhr-polling/:sid", hive_xhr_polling_handler, []} | build_dispatcher(Rest)].
