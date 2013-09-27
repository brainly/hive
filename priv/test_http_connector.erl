#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/jsonx/ebin deps/jesse/ebin deps/ibrowse/ebin deps/lager/ebin deps/goldrush/ebin deps/folsom/ebin deps/bear/ebin deps/poolboy/ebin -sname test_http_connector

-module(test_http_connector).

-define(DATA, "{\"sid\":\"1234567890\",\"trigger\":{\"name\":\"ping\",\"args\":[\"pong\"]},\"state\"{\"foo\":null}}").
-define(SIZE, 1000).
-define(TIMEOUT, 5000).

main([Type, NumRequests, Concurrency, Url]) ->
    main([Type, NumRequests, Concurrency, Url, ?DATA]);

main([Type, NumRequests, Concurrency, Url, DataString]) ->
    inets:start(),
    ibrowse:start(),
    lager:start(),
    folsom:start(),
    folsom_metrics:new_histogram(request_times, slide_uniform, {30, ?SIZE div 10}),
    Data = list_to_binary(DataString),
    case Type of
        "ibrowse"   -> test_ibrowse(list_to_integer(NumRequests), list_to_integer(Concurrency), Url, Data);
        "connector" -> test_connector(list_to_integer(NumRequests), list_to_integer(Concurrency), Url, Data);
        "httpc"     -> test_httpc(list_to_integer(NumRequests), list_to_integer(Concurrency), Url, Data)
    end,
    receive
        ok -> ok
    end;

main(Args) ->
    io:format("Usage: ./test_http_connector.erl TYPE NUMREQUESTS CONCURRENCY URL [DATA]"),
    halt(1).

%% Hive HTTP connector test:
test_connector(NumRequests, Concurrency, Url, Data) ->
    io:format("Testing Hive HTTP Connector: ~p requests to ~s (~p at the same time).~n", [NumRequests, Url, Concurrency]),
    {ok, {_Schema, _Info, Host, Port, Endpoint, _Query}} = http_uri:parse(Url),
    Config = [{<<"connector">>, <<"http">>},
              {<<"size">>, ?SIZE},
              {<<"overflow">>, 0},
              {<<"args">>, [{<<"base_url">>, list_to_binary("http://" ++ Host ++ ":" ++ integer_to_list(Port))},
                            {<<"max_connection_timeout">>, ?TIMEOUT},
                            {<<"max_connections">>, Concurrency}]}],
    application:set_env(hive, <<"connectors.pools">>, [{<<"test">>, Config}]),
    application:set_env(hive, <<"connectors.rent_timeout">>, 6000),
    {ok, Pid1} = hive_plugins_sup:start_link(),
    {ok, Pid2} = hive_connectors_sup:start_link(),
    timer:sleep(1000),
    spawn_connector(NumRequests, Endpoint, Data).

spawn_connector(NumRequests, Endpoint, Data) ->
    spawn_connector(spawn(fun() -> wait_for(NumRequests) end), NumRequests, NumRequests, Endpoint, Data).

spawn_connector(Ret, _TotalRequests, 0, _Endpoint, _Data) ->
    ok;

spawn_connector(Ret, TotalRequests, NumRequests, Endpoint, Data) ->
    spawn(fun() ->
                  send_connector_request(Ret, Endpoint, Data)
          end),
    spawn_connector(Ret, TotalRequests, NumRequests - 1, Endpoint, Data).

send_connector_request(Ret, Endpoint, Data) ->
    case
        begin
            Timer = erlang:start_timer(?TIMEOUT*2, self(), whatever),
            %% NOTE This is slow as fuck.
            %% Result = hive_connectors:do_safe(<<"test">>,
            %%                                   fun(Worker) ->
            %%                                           http_worker:sync_post(Worker, list_to_binary(Endpoint), Data)
            %%                                   end),
            Worker = hive_connectors:rent(<<"test">>),
            Result = http_worker:sync_post(Worker, list_to_binary(Endpoint), Data),
            hive_connectors:return(<<"test">>, Worker),
            Time = erlang:cancel_timer(Timer),
            folsom_metrics:notify(request_times, 2*?TIMEOUT - Time),
            Result
        end
    of
        {ok, _Result} ->
            Ret ! ok;

        Otherwise ->
            io:format("[error] connector error: ~p~n", [Otherwise]),
            Ret ! error
    end.

%% Ibrowse test:
test_ibrowse(NumRequests, Concurrency, Url, Data) ->
    io:format("Testing Ibrowse: ~p requests to ~s (~p at the same time).~n", [NumRequests, Url, Concurrency]),
    {ok, {_Schema, _Info, Host, Port, _Endpoint, _Query}} = http_uri:parse(Url),
    ibrowse:set_max_sessions(Host, Port, Concurrency),
    %%ibrowse:trace_on(Host, Port),
    spawn_ibrowse(NumRequests, Url, Data).

spawn_ibrowse(NumRequests, Url, Data) ->
    spawn_ibrowse(spawn(fun() -> wait_for(NumRequests) end), NumRequests, NumRequests, Url, Data).

spawn_ibrowse(Ret, _TotalRequests, 0, _Url, _Data) ->
    ok;

spawn_ibrowse(Ret, TotalRequests, NumRequests, Url, Data) ->
    spawn(fun() ->
                  send_ibrowse_request(Ret, Url, Data)
          end),
    spawn_ibrowse(Ret, TotalRequests, NumRequests - 1, Url, Data).

send_ibrowse_request(Ret, Url, Data) ->
    case
        begin
            Timer = erlang:start_timer(?TIMEOUT*2, self(), whatever),
            Result = ibrowse:send_req(Url,
                                      [],
                                      post,
                                      Data,
                                      [{connect_timeout, 1000},
                                       {response_format, binary}],
                                      ?TIMEOUT),
            Time = erlang:cancel_timer(Timer),
            folsom_metrics:notify(request_times, 2*?TIMEOUT - Time),
            Result
        end
    of
        {ok, _Status, _Headers, _Reply} ->
            Ret ! ok;

        Otherwise ->
            io:format("[error] ibrowse error: ~p~n", [Otherwise]),
            Ret ! error
    end.

%% HTTPC test:
test_httpc(NumRequests, Concurrency, Url, Data) ->
    io:format("Testing httpc: ~p requests to ~s (~p at the same time).~n", [NumRequests, Url, Concurrency]),
    httpc:set_options([{max_sessions, Concurrency}]),
    spawn_httpc(NumRequests, Url, Data).

spawn_httpc(NumRequests, Url, Data) ->
    spawn_httpc(spawn(fun() -> wait_for(NumRequests) end), NumRequests, NumRequests, Url, Data).

spawn_httpc(Ret, _TotalRequests, 0, _Url, _Data) ->
    ok;

spawn_httpc(Ret, TotalRequests, NumRequests, Url, Data) ->
    spawn(fun() ->
                  send_httpc_request(Ret, Url, Data)
          end),
    spawn_httpc(Ret, TotalRequests, NumRequests - 1, Url, Data).

send_httpc_request(Ret, Url, Data) ->
    case
        begin
            Timer = erlang:start_timer(?TIMEOUT*2, self(), whatever),
            Result = httpc:request(post,
                                   {Url, [], [], Data},
                                   [{connect_timeout, 1000},
                                    {timeout, ?TIMEOUT}],
                                   [{body_format, binary}]),
            Time = erlang:cancel_timer(Timer),
            folsom_metrics:notify(request_times, 2*?TIMEOUT - Time),
            Result
        end
    of
        {ok, _Reply} ->
            Ret ! ok;

        Otherwise ->
            io:format("[error] httpc error: ~p~n", [Otherwise]),
            Ret ! error
    end.

%% Statistics:
wait_for(NumRequests) ->
    wait_for(uptime(), NumRequests, NumRequests, 0, 0).

wait_for(StartTime, TotalRequests, 0, Ok, Errors) ->
    Time = case uptime() of
               StartTime -> 1;
               SomeTime  -> (SomeTime - StartTime)
           end,
    io:format("Results:~n~p~n",
              [[{ok, Ok},
                {errors, Errors},
                {ok_ratio, Ok / TotalRequests},
                {error_ratio, Errors / TotalRequests},
                {requests_per_second, 1000.0 * TotalRequests / Time},
                {request_times, folsom_metrics:get_histogram_statistics(request_times)}]]),
    halt(0);

wait_for(StartTime, TotalRequests, TotalRequests, Ok, Errors) ->
    receive
        ok    -> wait_for(uptime(), TotalRequests, TotalRequests - 1, Ok + 1, Errors);
        error -> wait_for(uptime(), TotalRequests, TotalRequests - 1, Ok, Errors + 1)
    end;

wait_for(StartTime, TotalRequests, NumRequests, Ok, Errors) ->
    case (TotalRequests - NumRequests) rem 1000 == 0 of
        true  -> io:format("Completed ~p requests.~n", [TotalRequests - NumRequests]);
        false -> ok
    end,
    receive
        ok    -> wait_for(StartTime, TotalRequests, NumRequests - 1, Ok + 1, Errors);
        error -> wait_for(StartTime, TotalRequests, NumRequests - 1, Ok, Errors + 1)
    end.

%% Utility functions:
uptime() ->
    element(1, erlang:statistics(wall_clock)).
