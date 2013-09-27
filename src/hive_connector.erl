-module(hive_connector).
-author('kajetan.rzepecki@zadane.pl').

-type reason() :: any().
-type args() :: [proplists:property()].

%% Used for pool-wide initialization of workers:
-callback common_init(PoolName :: binary(), Pool :: args()) ->
    {ok, PoolArgs :: args(), WorkerArgs :: args()} | {stop, reason()}.

%% Used for starting pools:
-callback start_pool(Pool :: args(), WorkerArgs :: args()) ->
    {ok, pid()} | {error, reason()}.

-callback checkout(Pool :: atom(), Timeout :: integer()) ->
    {ok, pid()} | {error, reason()}.

-callback checkin(Pool :: atom(), Worker :: pid()) ->
    ok | {error, reason()}.

-callback transaction(Pool :: atom(), Fun :: function()) ->
    any().

-callback stop_pool(Pool :: atom()) ->
    ok.
