#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/jsonx/ebin deps/jesse/ebin deps/ibrowse/ebin deps/lager/ebin deps/goldrush/ebin deps/folsom/ebin deps/bear/ebin deps/poolboy/ebin deps/ranch/ebin -sname test_http_connector

-module(test_tcp_connector).

-define(ENDPOINT, <<"test_endpoint">>).
-define(DATA, "{\"sid\":\"1234567890\",\"trigger\":{\"name\":\"ping\",\"args\":[\"pong\"]},\"trigger_id\":\"1+\",\"state\":{\"foo\":null}}").
-define(SIZE, 1).
-define(TIMEOUT, 5000).

main([Type, NumRequests, Concurrency, Port]) ->
    main([Type, NumRequests, Concurrency, Port, ?DATA]);

main([Type, NumRequests, Concurrency, Port, DataString]) ->
    inets:start(),
    lager:start(),
    folsom:start(),
    application:start(crypto),
    application:start(ranch),

    folsom_metrics:new_histogram(request_times, slide_uniform, {30, ?SIZE div 10}),
    Data = list_to_binary(DataString),
    case Type of
        "connector" -> test_connector(list_to_integer(NumRequests), list_to_integer(Concurrency), list_to_integer(Port), Data)
    end,
    receive
        ok -> ok
    end;

main(_Args) ->
    io:format("Usage: ./test_tcp_connector.erl TYPE NUMREQUESTS CONCURRENCY PORT [DATA]~n"),
    halt(1).

%% Hive HTTP connector test:
test_connector(NumRequests, Concurrency, Port, Data) ->
    io:format("Testing Hive TCP Connector: ~p requests on ~p (~p at the same time).~n", [NumRequests, Port, Concurrency]),
    Config = [{<<"connector">>, <<"connector.tcp">>},
              {<<"size">>, ?SIZE},
              {<<"overflow">>, 0},
              {<<"args">>, [{<<"port">>, Port},
                            {<<"max_connection_timeout">>, ?TIMEOUT},
                            {<<"restart_timeout">>, 10000},
                            {<<"max_connections">>, Concurrency}]}],
    application:set_env(hive, <<"connectors.pools">>, [{<<"test">>, Config}]),
    application:set_env(hive, <<"connectors.rent_timeout">>, 6000),
    application:set_env(hive, hive_plugins_dir, <<"plugins">>),
    application:set_env(hive, hive_config_file, <<"etc/hive.json">>),
    application:set_env(hive, hive_schema_dir, <<"etc/schema">>),

    ConfigPid = spawn_link(fun() -> mock_config() end),
    register(hive_config, ConfigPid),

    {ok, _Plugins} = hive_plugins_sup:start_link(),
    {ok, _Connectors} = hive_connectors_sup:start_link(),

    timer:sleep(1000),
    spawn_connector(NumRequests, Data).

spawn_connector(NumRequests, Data) ->
    spawn_connector(spawn(fun() -> wait_for(NumRequests) end), NumRequests, NumRequests, Data).

spawn_connector(_Ret, _TotalRequests, 0, _Data) ->
    ok;

spawn_connector(Ret, TotalRequests, NumRequests, Data) ->
    spawn(fun() ->
                  send_connector_request(Ret, Data)
          end),
    spawn_connector(Ret, TotalRequests, NumRequests - 1, Data).

send_connector_request(Ret, Data) ->
    case
        begin
            Timer = erlang:start_timer(?TIMEOUT*2, self(), whatever),
            {ok, Worker} = hive_connectors:rent(<<"test">>),
            Result = hive_protocol:post(Worker, ?ENDPOINT, Data),
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

wait_for(_StartTime, TotalRequests, TotalRequests, Ok, Errors) ->
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

mock_config() ->
    receive
        {'$gen_call', {Pid, Ref}, {validate, _Config, Config}} ->
            %%io:format("Config: ~p~n", [Config]),
            Pid ! {Ref, {ok, Config}}, %% NOTE gen:do_call mockup.
            mock_config();
        Otherwise ->
            %%io:format("[error] got unwanted message: ~p~n", [Otherwise]),
            mock_config()
    end.
