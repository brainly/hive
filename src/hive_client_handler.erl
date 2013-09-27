-module(hive_client_handler).
-author('kajetan.rzepecki@zadane.pl').

-type state() :: any().
-type reason() :: any().

-callback init(state()) ->
    {ok, state()} | {stop, reason()}.

-callback terminate(reason(), state()) ->
    ok.

%% Used to dispatch Erlang messages.
-callback info(any(), state()) ->
    {reply, any(), state()} | {noreply, state()} | {error, reason(), state()} | {stop, reason(), state()}.

%% Used to dispatch Hive messages,
-callback handle(any(), state()) ->
    {reply, any(), state()} | {noreply, state()} | {error, reason(), state()} | {stop, reason(), state()}.
