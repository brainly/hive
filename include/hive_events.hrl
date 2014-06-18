
-author('kajetan.rzepecki@zadane.pl').

%% Internal Hive & backend events.

%% Backend event.
-record(internal_event, {action = undefined :: binary(),
                         id     = undefined :: term(),
                         args   = undefined :: term()}).
