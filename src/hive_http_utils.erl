-module(hive_http_utils).
-author('kajetan.rzepecki@zadane.pl').

-export([authorize/2, reply/2, reply/3, reply_no_log/2, reply_no_log/3, origin_valid/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-define(HEADER(ORIGIN), [{<<"Origin">>, <<"*.*">>},
                         {<<"Content-Type">>, <<"text/plain; charset=UTF-8">>},
                         {<<"Access-Control-Allow-Credentials">>, <<"true">>},
                         {<<"Access-Control-Allow-Origin">>, ORIGIN},
                         {<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}
                        ]).

%% External functions:
reply(Reply, Request) ->
    reply(200, Reply, Request).

reply(Code, Reply, Request) ->
    log_reply(Code),
    {Origin, Req} = cowboy_req:header(<<"origin">>, Request, <<"">>),
    {ok, Req2} = cowboy_req:reply(Code, ?HEADER(Origin), Reply, Req),
    Req2.

%% NOTE These two functions are for Monitor & API use mainly,
%% NOTE since we don't care about HTTP request numbers there.
reply_no_log(Reply, Request) ->
    reply_no_log(200, Reply, Request).

reply_no_log(Code, Reply, Request) ->
    {ok, Req} = cowboy_req:reply(Code, [], Reply, Request),
    Req.

origin_valid(Request) ->
    case cowboy_req:header(<<"origin">>, Request, "null") of
        {Origin, _Req} ->
            case lists:member(Origin, hive_config:get(<<"hive.allowed_origins">>)) of
                true  -> {ok, Origin};
                false -> inc(?HTTP_ERRORS),
                         ErrorMsg = hive_error_utils:format("Origin ~s is not allowed to access Hive!", [Origin]),
                         lager:warning(ErrorMsg),
                         {error, {bad_origin, ErrorMsg}}
            end;
        _ ->
            inc(?HTTP_ERRORS),
            ErrorMsg = hive_error_utils:format("No origin specified in the request header!"),
            lager:warning(ErrorMsg),
            {error, {bad_origin, ErrorMsg}}
    end.

authorize(Hash, RHash) ->
    RealHash = hive_config:get(RHash),
    case Hash of
        RealHash -> ok;
        _        -> error
    end.

%% Internal functions:
log_reply(Code) when (Code >= 200) andalso (Code < 300) ->
    inc(?HTTP_2XX);

log_reply(Code) when (Code >= 400) andalso (Code < 500) ->
    inc(?HTTP_4XX);

log_reply(Code) when (Code >= 500) andalso (Code < 600)->
    inc(?HTTP_5XX);

log_reply(_Code) ->
    inc(?HTTP_UNKNOWN).
