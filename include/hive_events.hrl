-author('kajetan.rzepecki@zadane.pl').

%% Internal Hive & backend events.

%% Backend event.
-record(internal_event, {action = undefined :: binary(),
                         args   = undefined :: term()}).
