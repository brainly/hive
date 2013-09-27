-author('kajetan.rzepecki@zadane.pl').

%% Common client-worker stuff

%% Common client worker state record:
-record(state,
        {sink              = undefined :: pid() | term(),
         messages          = []        :: list(),
         poll_timer        = undefined :: reference(),
         hb_timer          = undefined :: reference(),
         session_timer     = undefined :: reference(),
         handler           = undefined :: atom(),
         client_state      = undefined :: term(),
         session_timeout   = undefined :: integer(),
         poll_timeout      = undefined :: integer(),
         heartbeat_timeout = undefined :: integer()}).
