-module(http_worker).
-author('kajetan.rzepecki@zadane.pl').

-export([stop/1, get/2, post/3, put/3]).

-include("hive_connectors.hrl").
-include("hive_monitor.hrl").

-define(inc(Counter), hive_monitor_utils:inc(Counter, State#state.pool_name)).
-define(dec(Counter), hive_monitor_utils:dec(Counter, State#state.pool_name)).

%% External functions:
stop(_State) ->
    ok.

get(State, Endpoint) ->
    ?inc(?CONN_HTTP_REQUESTS),
    ?inc(?CONN_HTTP_GET),
    {Args, _} = State#state.data,
    BaseUrl = proplists:get_value(<<"base_url">>, Args),
    Url = <<BaseUrl/binary, Endpoint/binary>>,
    Timeout = proplists:get_value(<<"max_connection_timeout">>, Args, 5000),
    dispatch(send_direct_request(Url, get, [], Timeout), State).

put(State, Endpoint, Data) ->
    ?inc(?CONN_HTTP_REQUESTS),
    ?inc(?CONN_HTTP_APOST),
    {Args, Sink} = State#state.data,
    BaseUrl = proplists:get_value(<<"base_url">>, Args),
    Url = <<BaseUrl/binary, Endpoint/binary>>,
    Timeout = proplists:get_value(<<"max_connection_timeout">>, Args, 5000),
    dispatch(send_direct_request(Url, post, Data, Sink, Timeout), State).

post(State, Endpoint, Data) ->
    ?inc(?CONN_HTTP_REQUESTS),
    ?inc(?CONN_HTTP_POST),
    {Args, _} = State#state.data,
    BaseUrl = proplists:get_value(<<"base_url">>, Args),
    Url = <<BaseUrl/binary, Endpoint/binary>>,
    Timeout = proplists:get_value(<<"max_connection_timeout">>, Args, 5000),
    dispatch(send_direct_request(Url, post, Data, Timeout), State).

%% Internal functions:
send_direct_request(Url, Method, Body, Timeout) ->
    ibrowse:send_req(binary_to_list(Url),
                     [],
                     Method,
                     Body,
                     [{connect_timeout, 100},
                      {inactivity_timeout, infinity},
                      {response_format, binary}],
                     Timeout).

send_direct_request(Url, Method, Body, ReturnTo, Timeout) ->
    ibrowse:send_req(binary_to_list(Url),
                     [],
                     Method,
                     Body,
                     [{connect_timeout, 100},
                      {inactivity_timeout, infinity},
                      {response_format, binary},
                      {stream_to, ReturnTo}],
                     Timeout).

dispatch(Reply, State) ->
    case Reply of
        %% FIXME Auto-retry here?
        {error, retry_later} ->
            ?inc(?CONN_HTTP_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive HTTP Hook request failed: ~p", [retry_later]),
            lager:warning(ErrorMsg),
            {error, retry_later};

        {error, Error} ->
            ?inc(?CONN_HTTP_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive HTTP Hook request failed: ~p", [Error]),
            lager:warning(ErrorMsg),
            {error, {http_error, ErrorMsg}};

        {ok, "200", _Headers, Body} ->
            {ok, Body};

        {ok, Status, _Headers, _Body} ->
            ?inc(?CONN_HTTP_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive HTTP Hook request failed: unhandled HTTP reply status code - ~s.",
                                                [Status]),
            lager:warning(ErrorMsg),
            {error, {http_error, ErrorMsg}};

        {ibrowse_req_id, _RequestId} ->
            ok
    end.
