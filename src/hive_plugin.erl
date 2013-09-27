-module(hive_plugin).
-author('kajetan.rzepecki@zadane.pl').

-type args() :: any().
-type reason() :: any().
-type state() :: any().

%% Used to initialize the plugin and return its executable version.
-callback load() ->
    {ok, [{binary(), function()}], state()} | {stop, reason()}.

-callback unload(State :: state()) ->
    ok.

-callback validate(Name :: any(), Args :: args()) ->
    ok | {error, reason()}.

%% NOTE Hooks:
%% -spec hook_init(args() ->
%%    {ok, function()} | {stop, reason()}

%% -spec hook_function(args(), binary(), #sio_message{}, client_state()) ->
%%    {noreply, client_state()}
%%  | {reply, #sio_message{}, client_state()}
%%  | {reply, [#sio_message{}], client_state(}
%%  | {error, reason(), client_state()}
%%  | {stop, reason(), client_state()}.

%% NOTE Internal Event handlers:
%% -spec event_init(args() ->
%%    {ok, function()} | {stop, reason()}

%% -spec event_function(args(), binary(), #internal_event{}, client_state()) ->
%%    {noreply, client_state()}
%%  | {reply, #sio_message{}, client_state()}
%%  | {reply, [#sio_message{}], client_state(}
%%  | {error, reason(), client_state()}
%%  | {stop, reason(), client_state()}.

%% NOTE Connectors:
%% -spec connector_init(args()) ->
%%    {ok, atom()} | {stop, reason()}

%% NOTE State Managers:
%% -spec state_manager_init(client_state()) ->
%%    {ok, client_state()}
%%  | {stop, reason()}.

%% -spec getter_function(term(), client_state()) ->
%%    {ok, term(), client_state()}
%%  | {error, reason(), client_state()}
%%  | {stop, reason(), client_state()}.

%% -spec setter_function(term(), term(), client_state()) ->
%%    {ok, term(), client_state()}
%%  | {error, reason(), client_state()}
%%  | {stop, reason(), client_state()}.

%% -spec cleanup_function(client_state()) ->
%%    {ok, term(), client_state()}
%%  | {error, reason(), client_state()}
%%  | {stop, reason(), client_state()}.
