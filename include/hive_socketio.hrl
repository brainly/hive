-author('kajetan.rzepecki@zadane.pl').

%% Socket.IO stuff

-define(MESSAGE_TYPES, [disconnect, connect, heartbeat, message, json, event, ack, error, noop]).

-define(DISCONNECT, <<"0:::">>).
-define(CONNECT, <<"1:::">>).
-define(HEARTBEAT, <<"2:::">>)
-define(MESSAGE (Message), list_to_binary(io_lib:format("3:::~s", [Message]))).
-define(JSON (Json), list_to_binary(io_lib:format("4:::~s", [Json]))).
-define(EVENT (Event), list_to_binary(io_lib:format("5:::~s", [Event]))).
-define(ACK (Id, Event), list_to_binary(io_lib:format("6:::~s+[~s]", [Id, Event]))).
-define(ERROR (Error), list_to_binary(io_lib:format("7:::~s", [Error]))).
-define(NOOP, <<"8:::">>).

%% [message type] ':' [message id ('+')] ':' [message endpoint] (':' [message data])
-record(sio_message,
        {type     = noop      :: 'disconnect' | 'connect' | 'heartbeat' | 'message' | 'json' | 'event' | 'ack' | 'error' | 'noop',
         id       = undefined :: 'undefined' | {'client', integer()} | {'server', integer()}, %% NOTE + is for client to handle.
         endpoint = <<"">>    :: binary(),
         data     = <<"">>    :: binary()}).
