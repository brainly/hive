-module(hive_pubsub_channel_tests).
-author('kajetan.rzepecki@brainly.com').

-include_lib("eunit/include/eunit.hrl").

-include("hive_monitor.hrl").

start_channel(Prefix, Name, Timeout) ->
    IncCounters = [?PUBSUB_CHANNEL_CHANNELS, ?PUBSUB_CHANNEL_REQUESTS, ?PUBSUB_CHANNEL_STATUS,
                   ?PUBSUB_CHANNEL_SUBS, ?PUBSUB_CHANNEL_UNSUBS, ?PUBSUB_CHANNEL_SUBSCR],
    DecCounters = [?PUBSUB_CHANNEL_SUBSCR],
    IncbyCounters = [?PUBSUB_CHANNEL_PUBLISHED],
    lists:foreach(fun({FunName, ExpectedCounters}) ->
                          meck:expect(hive_monitor_utils, FunName,
                                      fun(CounterName, P) when P =:= Prefix ->
                                              case lists:member(CounterName, ExpectedCounters) of
                                                  true  -> ok;
                                                  false -> {error, bad_counter}
                                              end
                                      end)
                  end,
                  [{inc, IncCounters},
                   {dec, DecCounters}]),
    meck:expect(hive_monitor_utils, name,
                fun(CounterName, P) when P =:= Prefix ->
                        case lists:member(CounterName, IncbyCounters) of
                            true  -> meck:passthrough([CounterName, P]);
                            false -> {error, bad_counter}
                        end
                end),
    meck:expect(hive_monitor_utils, incby, fun(_Counter, _Value) -> ok end),
    hive_pubsub_channel:start_link(Prefix, Name, Timeout).

validate(Modules) ->
    case catch meck:validate(Modules) of
        {bad_return_value, bad_counter} -> false;
        Otherwise                       -> Otherwise
    end.

client_fun() ->
    receive
        _Anything -> client_fun()
    end.

start_client() ->
    meck:expect(hive_client, process_events, fun(_Pid, _Events) -> ok end),
    {ok, spawn(fun client_fun/0)}.

start_test_() ->
    RequiredModules = [hive_monitor_utils],
    meck:new(RequiredModules),
    {Ok, Pid} = start_channel(<<"prefix">>, <<"name">>, 0),
    timer:sleep(200),
    Status = process_info(Pid),
    Meck = validate(RequiredModules),
    meck:unload(RequiredModules),
    [?_assertEqual(ok, Ok),
     ?_assert(Status =/= undefined),
     ?_assert(Meck)].

subscribe_test_() ->
    RequiredModules = [hive_monitor_utils, hive_client],
    meck:new(RequiredModules),
    {ok, Client1} = start_client(),
    {ok, Client2} = start_client(),
    {ok, Channel} = start_channel(<<"prefix">>, <<"name">>, 0),
    Ok1 = hive_pubsub_channel:subscribe(Channel, Client1),
    Ok2 = hive_pubsub_channel:subscribe(Channel, Client2),
    Ok3 = hive_pubsub_channel:subscribe(Channel, Client1),
    Ok4 = hive_pubsub_channel:subscribe(Channel, Client2),
    Meck = validate(RequiredModules),
    meck:unload(RequiredModules),
    [?_assertEqual(ok, Ok1),
     ?_assertEqual(ok, Ok2),
     ?_assertEqual(ok, Ok3),
     ?_assertEqual(ok, Ok4),
     ?_assert(Meck)].

unsubscribe_test_() ->
    RequiredModules = [hive_monitor_utils, hive_client],
    meck:new(RequiredModules),
    {ok, Client} = start_client(),
    {ok, Channel} = start_channel(<<"prefix">>, <<"name">>, 200),
    Ok1 = hive_pubsub_channel:subscribe(Channel, Client),
    Ok2 = hive_pubsub_channel:unsubscribe(Channel, Client),
    Ok3 = hive_pubsub_channel:unsubscribe(Channel, Client),
    Meck = validate(RequiredModules),
    meck:unload(RequiredModules),
    [?_assertEqual(ok, Ok1),
     ?_assertEqual(ok, Ok2),
     ?_assertEqual(ok, Ok3),
     ?_assert(Meck)].

publish_test_() ->
    RequiredModules = [hive_monitor_utils, hive_client],
    meck:new(RequiredModules),
    {ok, Client1} = start_client(),
    {ok, Client2} = start_client(),
    {ok, Channel} = start_channel(<<"prefix">>, <<"name">>, 0),
    ok = hive_pubsub_channel:subscribe(Channel, Client1),
    ok = hive_pubsub_channel:subscribe(Channel, Client2),
    Ok = hive_pubsub_channel:publish(Channel, [<<"msg">>]),
    History = meck:history(hive_client),
    HistoryOk = lists:all(fun({_Pid, {hive_client, process_events, [C, [<<"msg">>]]}, ok}) when C =:= Client1; C =:= Client2 ->
                                  true;

                             (_Otherwise)->
                                  false
                          end,
                          History),
    Meck = validate(RequiredModules),
    meck:unload(RequiredModules),
    [?_assertEqual(ok, Ok),
     ?_assert(HistoryOk),
     ?_assertEqual(2, lists:length(History)),
     ?_assert(Meck)].

timeout_test_() ->
    RequiredModules = [hive_monitor_utils, hive_client],
    meck:new(RequiredModules),
    {ok, Client} = start_client(),
    {ok, Channel} = start_channel(<<"prefix">>, <<"name">>, 200),
    ok = hive_pubsub_channel:subscribe(Channel, Client),
    timer:sleep(500),
    Status1 = process_info(Channel),
    ok = hive_pubsub_channel:unsubscribe(Channel, Client),
    timer:sleep(500),
    Status2 = process_info(Channel),
    Meck = validate(RequiredModules),
    meck:unload(RequiredModules),
    [?_assert(Status1 =/= undefined),
     ?_assertEqual(undefined, Status2),
     ?_assert(Meck)].

status_test_() ->
    RequiredModules = [hive_monitor_utils, hive_client],
    meck:new(RequiredModules),
    {ok, Client1} = start_client(),
    {ok, Client2} = start_client(),
    {ok, Channel} = start_channel(<<"prefix">>, <<"name">>, 0),
    Zero = hive_pubsub_channel:status(Channel),
    Ok1 = hive_pubsub_channel:subscribe(Channel, Client1),
    One = hive_pubsub_channel:status(Channel),
    Ok2 = hive_pubsub_channel:subscribe(Channel, Client1),
    AnotherOne = hive_pubsub_channel:status(Channel),
    Ok3 = hive_pubsub_channel:subscribe(Channel, Client2),
    Two = hive_pubsub_channel:status(Channel),
    Ok4 = hive_pubsub_channel:unsubscribe(Channel, Client1),
    YetAnotherOne = hive_pubsub_channel:status(Channel),
    Ok5 = hive_pubsub_channel:unsubscribe(Channel, Client1),
    StillOne = hive_pubsub_channel:status(Channel),
    Ok6 = hive_pubsub_channel:unsubscribe(Channel, Client2),
    AnotherZero = hive_pubsub_channel:status(Channel),
    Meck = validate(RequiredModules),
    meck:unload(RequiredModules),
    [?_assertEqual(ok, Ok1),
     ?_assertEqual(ok, Ok2),
     ?_assertEqual(ok, Ok3),
     ?_assertEqual(ok, Ok4),
     ?_assertEqual(ok, Ok5),
     ?_assertEqual(ok, Ok6),
     ?_assertEqual(0, Zero),
     ?_assertEqual(1, One),
     ?_assertEqual(1, AnotherOne),
     ?_assertEqual(2, Two),
     ?_assertEqual(1, YetAnotherOne),
     ?_assertEqual(1, StillOne),
     ?_assertEqual(0, AnotherZero),
     ?_assert(Meck)].
