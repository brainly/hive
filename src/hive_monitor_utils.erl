-module(hive_monitor_utils).
-author('kajetan.rzepecki@zadane.pl').

-export([inc/2, inc/1, incby/2, dec/2, dec/1, decby/2, name/2, init_counters/1]).

%% External functions:
init_counters(Counters) ->
    hive_monitor:init(Counters).

inc(Counter) ->
    hive_monitor:inc(Counter).

inc(Counter, Name) ->
    mod(inc, Counter, Name).

incby(Counter, By) ->
    hive_monitor:inc(Counter, By).

decby(Counter, By) ->
    hive_monitor:dec(Counter, By).

dec(Counter) ->
    hive_monitor:dec(Counter).

dec(Counter, Name) ->
    mod(dec, Counter, Name).

name(Counter, Name) ->
    binary:replace(Counter, [<<"$(name)">>], Name).

%% Internal functions:
mod(Fun, Counter, Name) ->
    hive_monitor:Fun(name(Counter, Name)).
