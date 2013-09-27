-module(hive_router_api_handler).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, info/3, terminate/3]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).
-import(hive_http_utils, [authorize/2, reply_no_log/2, reply_no_log/3]).
-import(hive_error_utils, [make_json/2, format/2]).

%% HTTP handler callbacks:
init({tcp, http}, Request, _Options) ->
    inc(?API_REQUESTS),
    inc(?API_ROUTER_REQUESTS),
    {Hash, Request0} = cowboy_req:binding(apihash, Request),
    case authorize(Hash, <<"api.hash">>) of
        ok ->
            {Action, Request1} = cowboy_req:binding(action, Request0),
            {ok, Request1, Action};

        error ->
            inc(?API_ERRORS),
            inc(?API_ROUTER_ERRORS),
            Msg = "Unauthorized Hive API access attempt!",
            lager:warning(Msg),
            Req = reply_no_log(401, make_json(bad_api_request, Msg), Request0),
            {shutdown, Req, error}
    end.

terminate(_Reason, _Request, _State) ->
    ok.

%% HTTP handler handlers:
handle(Request, <<"terminate">>) ->
    case cowboy_req:method(Request) of
        {<<"POST">>, Req} ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            Time = hive_config:get(<<"hive.graceful_termination_timeout">>),
            hive_router:terminate_after(Time, Body),
            Req3 = reply_no_log(<<"">>, Req2),
            {ok, Req3, done};

        {Method, Req} ->
            inc(?API_ERRORS),
            inc(?API_ROUTER_ERRORS),
            ErrorMsg = format("Unsupported Hive API access method: ~s.", [Method]),
            lager:warning(ErrorMsg),
            Req2 = reply_no_log(405, make_json(bad_api_request, ErrorMsg), Req),
            {ok, Req2, error}
    end;

handle(Request, <<"enable">>) ->
    case cowboy_req:method(Request) of
        {<<"POST">>, Req} ->
            hive_router:enable(),
            {ok, reply_no_log(<<"">>, Req), done};

        {Method, Req} ->
            inc(?API_ERRORS),
            inc(?API_ROUTER_ERRORS),
            ErrorMsg = format("Unsupported Hive API access method: ~s.", [Method]),
            lager:warning(ErrorMsg),
            Req2 = reply_no_log(405, make_json(bad_api_request, ErrorMsg), Req),
            {ok, Req2, error}
    end;

handle(Request, <<"disable">>) ->
    case cowboy_req:method(Request) of
        {<<"POST">>, Req} ->
            hive_router:disable(),
            {ok, reply_no_log(<<"">>, Req), done};

        {Method, Req} ->
            inc(?API_ERRORS),
            inc(?API_ROUTER_ERRORS),
            ErrorMsg = format("Unsupported Hive API access method: ~s.", [Method]),
            lager:warning(ErrorMsg),
            Req2 = reply_no_log(405, make_json(bad_api_request, ErrorMsg), Req),
            {ok, Req2, error}
    end;

handle(Request, Action) ->
    inc(?API_ERRORS),
    inc(?API_ROUTER_ERRORS),
    ErrorMsg = format("Unhandled Hive API action: ~p", [Action]),
    lager:warning(ErrorMsg),
    Req = reply_no_log(501, make_json(bad_api_request, ErrorMsg), Request),
    {ok, Req, Action}.

info(Info, Request, State) ->
    inc(?API_ERRORS),
    inc(?API_ROUTER_ERRORS),
    lager:warning("Unhandled Hive API info message: ~p", [Info]),
    %% NOTE We don't expect any messages, but it's not really worth killing the request if we get one.
    {loop, Request, State, hibernate}.
