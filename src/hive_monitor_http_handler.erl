-module(hive_monitor_http_handler).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, info/3, terminate/3]).

-include("hive_monitor.hrl").
-import(hive_http_utils, [authorize/2, reply_no_log/2, reply_no_log/3]).
-import(hive_error_utils, [make_json/2, format/2]).

%% HTTP handler callbacks:
init({tcp, http}, Request, _Options) ->
    {Hash, Request0} = cowboy_req:binding(hash, Request),
    case authorize(Hash, <<"monitor.hash">>) of
        ok ->
            {Prefix, Request1} = cowboy_req:binding(prefix, Request0),
            {ok, Request1, {dispatch, Prefix}};

        error ->
            Msg = <<"Unauthorized Hive Monitor access attempt!">>,
            lager:warning(Msg),
            Req = reply_no_log(401, make_json(bad_monitor_request, Msg), Request0),
            {shutdown, Req, error}
    end.

terminate(_Reason, _Request, _State) ->
    ok.

%% HTTP handler handlers:
handle(Request, {dispatch, Prefix}) ->
    update_gouges(),
    case cowboy_req:method(Request) of
        {<<"GET">>, Req} ->
            case hive_monitor:get(Prefix) of
                {[]}   -> ErrorMsg = hive_error_utils:format("Requested metric \"~s\" does not exist.", [Prefix]),
                           Req2 = reply_no_log(404, make_json(bad_monitor_request, ErrorMsg), Req),
                           {ok, Req2, error};

                Metrics -> Msg = jsonx:encode(Metrics),
                           Req2 = reply_no_log(Msg, Req),
                           {ok, Req2, done}
            end;

        {<<"DELETE">>, Req} ->
            %% NOTE Assumes Prefix to be a single value
            hive_monitor:reset(Prefix),
            Req2 = reply_no_log(<<"">>, Req),
            {ok, Req2, done};

        {Method, Req} ->
            ErrorMsg = hive_error_utils:format("Unsupported Hive Monitor access method: ~s.", [Method]),
            lager:warning(ErrorMsg),
            Req2 = reply_no_log(405, make_json(bad_monitor_request, ErrorMsg), Req),
            {ok, Req2, error}
    end;

handle(Request, Action) ->
    Msg = format("Unhandled Hive Monitor request: ~p", [Action]),
    lager:warning(Msg),
    Req = reply_no_log(501, make_json(bad_monitor_request, Msg), Request),
    {ok, Req, Action}.

info(Info, Request, State) ->
    lager:warning("Unhandled Hive Monitor info message: ~p", [Info]),
    {loop, Request, State, hibernate}.

%% Internal functions:
update_gouges() ->
    %% NOTE Updates all the non-incremental counters arround the system on demand.
    Nodes = hive_cluster:connected_nodes(),
    hive_monitor:set(?CLUSTER_SIZE, length(Nodes)),
    hive_monitor:log(?CLUSTER_NODES, Nodes),
    hive_monitor:set(?ROUTER_UPTIME, hive_router:uptime()),
    hive_monitor:set(?ROUTER_QUEUE, hive_router:msg_queue()),
    hive_monitor:set(?PUBSUB_UPTIME, hive_pubsub:uptime()),
    hive_monitor:set(?HIVE_UPTIME, hive:uptime()),
    hive_monitor:set(?TOTAL_PROCESSES, hive:processes()),
    Memory = hive:memory(),
    hive_monitor:set(?TOTAL_MEMORY, proplists:get_value(total, Memory)),
    hive_monitor:set(?PROCESSES_MEMORY, proplists:get_value(processes, Memory)),
    hive_monitor:set(?SYSTEM_MEMORY, proplists:get_value(system, Memory)),
    hive_monitor:set(?ATOM_MEMORY, proplists:get_value(atom, Memory)),
    hive_monitor:set(?BINARY_MEMORY, proplists:get_value(binary, Memory)),
    hive_monitor:set(?CODE_MEMORY, proplists:get_value(code, Memory)),
    hive_monitor:set(?ETS_MEMORY, proplists:get_value(ets, Memory)).
