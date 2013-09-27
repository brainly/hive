-module(hive_protocol).
-author('kajetan.rzepecki@zadane.pl').

-export([get/2, post/3, put/3]).

%% The Hive communication protocol
%% External functions:
get(Worker, Endpoint) ->
    %% NOTE This is used to retrieve some data from the Endpoint.
    call(Worker, {get, Endpoint}).

put(Worker, Endpoint, Data) ->
    %% NOTE This one is used to send some data to the Endpoint.
    call(Worker, {put, Endpoint, Data}).

post(Worker, Endpoint, Data) ->
    %% NOTE This is used to send and retrieve some data from the Endpoint.
    call(Worker, {post, Endpoint, Data}).

%% Internal functions:
call({WorkerModule, WorkerState}, {Message, Arg}) ->
    %% NOTE This one is used when a worker process that implements
    %% NOTE the Hive Protocol isn't a gen_server.
    WorkerModule:Message(WorkerState, Arg);

call({WorkerModule, WorkerState}, {Message, ArgA, ArgB}) ->
    %% NOTE This one is used when a worker process that implements
    %% NOTE the Hive Protocol isn't a gen_server.
    WorkerModule:Message(WorkerState, ArgA, ArgB);

call(Worker, Message) ->
    gen_server:call(Worker, Message).
