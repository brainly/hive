-module(hive_monitor).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([set/2, reset/1, get/0, get/1]).
-export([inc/1, inc/2, dec/1, dec/2]).

-include("hive_monitor.hrl").

%% Gen Server callbacks:
start_link() ->
    application:start(folsom),
    lager:notice("Starting Hive Monitor..."),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, real_init, []) of
        {error, Error} -> inc(?HIVE_ERRORS),
                          ErrorMsg = hive_error_utils:format("Cannot start Hive Monitor: ~p", [Error]),
                          lager:error(ErrorMsg),
                          {error, {hive_error, ErrorMsg}};
        Ret            -> lager:notice("Hive Monitor started!"),
                          Ret
    end.

terminate(_Reason, _State) ->
    ok.

init(real_init) ->
    %% FIXME Derp.
    init(?COUNTERS),
    {ok, undefined};

%% External API:
%% NOTE Makes no use of the Monitor server.

init(Counters) when is_list(Counters) ->
    %% FIXME Rename this.
    lists:foreach(fun init/1, Counters);

init(Counter) ->
    folsom_metrics:new_counter(Counter).

get() ->
    get_metrics(<<"">>).

get(undefined) ->
    get_metrics(<<"">>);

get(Prefix) ->
    get_metrics(Prefix).

reset(Counter) ->
    set(Counter, 0).

set(Counter, Value) ->
    folsom_metrics:notify(Counter, clear, counter),
    folsom_metrics:notify(Counter, {inc, Value}, counter).

inc(Counter) ->
    inc(Counter, 1).

inc(Counter, Value) ->
    folsom_metrics:notify(Counter, {inc, Value}, counter).

dec(Counter) ->
    dec(Counter, 1).

dec(Counter, Value) ->
    folsom_metrics:notify(Counter, {dec, Value}, counter).

%% Gen Server handlers:
handle_call(Msg, _From, State) ->
    inc(?HIVE_PLUGIN_ERRORS),
    lager:warning("Unhandled Hive Monitor call: ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    inc(?HIVE_PLUGIN_ERRORS),
    lager:warning("Unhandled Hive Monitor cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    inc(?HIVE_PLUGIN_ERRORS),
    lager:warning("Unhandled Hive Monitor info: ~p.", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    inc(?HIVE_PLUGIN_ERRORS),
    lager:warning("Unhandled Hive Monitor code change."),
    {ok, State}.

%% Internal functions:
get_metrics(Prefix) ->
    Metrics = folsom_metrics:get_metrics(),
    Values = lists:map(fun folsom_metrics:get_metric_value/1, Metrics),
    MVPairs = lists:zip(Metrics, Values),
    structurize(filter_metrics(split(Prefix), lists:map(fun({M, V}) ->
                                                                split(M) ++ [V]
                                                        end,
                                                        MVPairs))).

filter_metrics(Pattern, Metrics) ->
    lists:filter(fun(Metric) ->
                         matches(Pattern, Metric)
                 end,
                 Metrics).

structurize(Metrics) ->
    group(lists:sort(Metrics)).

split(Pattern) ->
    binary:split(Pattern, <<".">>, [global]).

matches([<<"">>], _Value) ->
    true;

matches(_Pattern, []) ->
    false;

matches([], _Value) ->
    true;

matches([P | Pattern], [P | Value]) ->
    matches(Pattern, Value);

matches(_Pattern, _Value) ->
    false.

%% Pattern matching disaster immenient...
group(Value) when not(is_list(Value))->
    Value;

group(Metrics) ->
    {lists:map(fun({Name, Groups}) ->
                       {Name, group(Groups)}
               end,
               lists:foldl(fun([Key, Value], OtherGroups) ->
                                   [{Key, Value} | OtherGroups];

                              ([Group | Stuff], [{Group, OtherStuff} | OtherGroups]) ->
                                   [{Group, [Stuff | OtherStuff]} | OtherGroups];

                              ([Group | Stuff], OtherGroups) ->
                                   [{Group, [Stuff]} | OtherGroups]
                           end,
                           [],
                           Metrics))}.
