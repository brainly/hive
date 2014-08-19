-module(hive_pubsub_api_handler).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, info/3, terminate/3]).

-include("hive_socketio.hrl").
-include("hive_events.hrl").
-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).
-import(hive_http_utils, [authorize/2, reply_no_log/2, reply_no_log/3]).
-import(hive_error_utils, [make_json/2, format/2]).

%% HTTP handler callbacks:
init({tcp, http}, Request, _Options) ->
    inc(?API_REQUESTS),
    inc(?API_PUBSUB_REQUESTS),
    {Hash, Request0} = cowboy_req:binding(apihash, Request),
    case authorize(Hash, <<"api.hash">>) of
        ok ->
            {Action, Request1} = cowboy_req:binding(action, Request0),
            {Id, Request2} = cowboy_req:binding(id, Request1),
            {ok, Request2, {Action, Id}};

        error ->
            inc(?API_ERRORS),
            inc(?API_PUBSUB_ERRORS),
            Msg = "Unauthorized Hive API access attempt!",
            lager:warning(Msg),
            Req = reply_no_log(401, make_json(bad_api_request, Msg), Request0),
            {shutdown, Req, error}
    end.

terminate(_Reason, _Request, _State) ->
    ok.

%% HTTP handler handlers:
handle(Request, {<<"action">>, Cid}) ->
    case cowboy_req:method(Request) of
        {<<"POST">>, Req} ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            case hive_events:parse(Body) of
                {ok, Events} ->
                    lager:info("Hive API Server publishing actions on channel ~s: ~p", [Cid, Events]),
                    case hive_pubsub:publish([Cid], Events) of
                        ok ->
                            Req3 = reply_no_log(<<"">>, Req2),
                            {ok, Req3, done};

                        {error, {Code, Error}} ->
                            inc(?API_ERRORS),
                            inc(?API_PUBSUB_ERRORS),
                            lager:debug("Hive API Server encountered an error: ~p", [{Code, Error}]),
                            Req3 = reply_no_log(400, make_json(Code, Error), Req2),
                            {ok, Req3, error}
                    end;

                {error, {Code, Error}} ->
                    inc(?API_ERRORS),
                    inc(?API_PUBSUB_ERRORS),
                    lager:debug("Hive API Server encountered an error: ~p", [{Code, Error}]),
                    Req3 = reply_no_log(400, make_json(Code, Error), Req2),
                    {ok, Req3, error}
            end;

        {Method, Req} ->
            inc(?API_ERRORS),
            inc(?API_PUBSUB_ERRORS),
            ErrorMsg = format("Unsupported Hive API access method: ~s.", [Method]),
            lager:warning(ErrorMsg),
            Req2 = reply_no_log(405, make_json(bad_api_request, ErrorMsg), Req),
            {ok, Req2, error}
    end;

handle(Request, {<<"publish">>, Cid}) ->
    case cowboy_req:method(Request) of
        {<<"POST">>, Req} ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            case jsonx:decode(Body, [{format, proplist}]) of
                {error, Error} ->
                    inc(?API_ERRORS),
                    inc(?API_PUBSUB_ERRORS),
                    lager:debug("Hive API Server encountered an error: ~p", [{bad_external_event, Error}]),
                    Req3 = reply_no_log(400, make_json(bad_external_event, Error), Req2),
                    {ok, Req3, error};

                Event ->
                    case hive_config:validate(<<"external_event">>, Event) of
                        {ok, _} ->
                            lager:info("Hive API Server publishing event on channel ~s: ~s", [Cid, Body]),
                            Message = hive_socketio_parser:encode(#sio_message{type = event, data = Body}),
                            hive_pubsub:publish([Cid], Message),
                            Req3 = reply_no_log(<<"">>, Req2),
                            {ok, Req3, done};

                        {error, {Code, Error}} ->
                            inc(?API_ERRORS),
                            inc(?API_PUBSUB_ERRORS),
                            lager:debug("Hive API Server encountered an error: ~p", [{Code, Error}]),
                            Req3 = reply_no_log(400, make_json(Code, Error), Req2),
                            {ok, Req3, error}
                    end
            end;

        {Method, Req} ->
            inc(?API_ERRORS),
            inc(?API_PUBSUB_ERRORS),
            ErrorMsg = format("Unsupported Hive API access method: ~s.", [Method]),
            lager:warning(ErrorMsg),
            Req2 = reply_no_log(405, make_json(bad_api_request, ErrorMsg), Req),
            {ok, Req2, error}
    end;

handle(Request, {<<"subscribe">>, Id}) ->
    case cowboy_req:method(Request) of
        {<<"GET">>, Req} ->
            case hive_pubsub:status(Id) of
                {ok, NumActiveClients} ->
                    Req2 = reply_no_log(jsonx:encode({[{subscribed_clients, NumActiveClients}]}), Req),
                    {ok, Req2, done};

                {error, {Code, Error}} ->
                    inc(?API_ERRORS),
                    inc(?API_PUBSUB_ERRORS),
                    lager:debug("Hive API Server encountered an error: ~p", [{Code, Error}]),
                    Req2 = reply_no_log(400, make_json(Code, Error), Req),
                    {ok, Req2, error}
            end;

        {<<"POST">>, Req} ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            case jsonx:decode(Body) of
                Cids = [Cid | _Rest] when is_binary(Cid) ->
                    case hive_pubsub:subscribe(Id, Cids) of
                        ok ->
                            Req3 = reply_no_log(<<"">>, Req2),
                            {ok, Req3, done};

                        {error, {Code, Error}} ->
                            inc(?API_ERRORS),
                            inc(?API_PUBSUB_ERRORS),
                            lager:debug("Hive API Server encountered an error: ~p", [{Code, Error}]),
                            Req3 = reply_no_log(500, make_json(Code, Error), Req2),
                            {ok, Req3, error}
                    end;

                _Other ->
                    inc(?API_ERRORS),
                    inc(?API_PUBSUB_ERRORS),
                    ErrorMsg = format("Hive API Server received invalid data: ~s", [Body]),
                    lager:error(ErrorMsg),
                    Req3 = reply_no_log(400, make_json(bad_request, ErrorMsg), Req),
                    {ok, Req3, error}
            end;

        {<<"DELETE">>, Req} ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            case jsonx:decode(Body) of
                Cids = [Cid | _Rest] when is_binary(Cid) ->
                    case hive_pubsub:unsubscribe(Id, Cids) of
                        ok ->
                            Req3 = reply_no_log(<<"">>, Req2),
                            {ok, Req3, done};

                        {error, {Code, Error}} ->
                            inc(?API_ERRORS),
                            inc(?API_PUBSUB_ERRORS),
                            lager:debug("Hive API Server encountered an error: ~p", [{Code, Error}]),
                            Req3 = reply_no_log(500, make_json(Code, Error), Req2),
                            {ok, Req3, error}
                    end;

                _Other ->
                    inc(?API_ERRORS),
                    inc(?API_PUBSUB_ERRORS),
                    ErrorMsg = format("Hive API Server received invalid data: ~s", [Body]),
                    lager:error(ErrorMsg),
                    Req3 = reply_no_log(400, make_json(bad_request, ErrorMsg), Req),
                    {ok, Req3, error}
            end;

        {Method, Req} ->
            inc(?API_ERRORS),
            inc(?API_PUBSUB_ERRORS),
            ErrorMsg = format("Unsupported Hive API Server access method: ~s.", [Method]),
            lager:warning(ErrorMsg),
            Req2 = reply_no_log(405, make_json(bad_api_request, ErrorMsg), Req),
            {ok, Req2, error}
    end;

handle(Request, Action) ->
    inc(?API_ERRORS),
    inc(?API_PUBSUB_ERRORS),
    lager:warning("Unhandled Hive API Server request: ~p", [Action]),
    {ok, Request, Action}.

info(Info, Request, State) ->
    inc(?API_ERRORS),
    inc(?API_PUBSUB_ERRORS),
    lager:warning("Unhandled Hive API Server info message: ~p", [Info]),
    {loop, Request, State, hibernate}.

