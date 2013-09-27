-author('kajetan.rzepecki@zadane.pl').

%% Common Hive Connectors related stuff.

-record(state, {lock                  :: locked | unlocked | stopped,
                pool_name             :: binary(),
                data                  :: term(),
                reconnect_timeout     :: integer(),
                max_reconnect_timeout :: integer(),
                restart_timeout       :: integer(),
                restart_timer}).
