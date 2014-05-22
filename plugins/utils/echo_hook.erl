-module(echo_hook).
-author('kajetan.rzepecki@zadane.pl').

-behaviour(hive_plugin).

-export([load/0, unload/1, validate/2]).

-include("hive_socketio.hrl").

load() ->
    {ok, [{<<"utils.echo">>, fun init/1}], undefined}.

unload(_State) ->
    ok.

validate(<<"utils.echo">>, _Hook) ->
    ok.

init(_Args) ->
    {ok, fun echo/4}.

echo(null, _Event, Trigger, State) ->
    case Trigger#sio_message.id of
        undefined -> {reply, Trigger, State};
        Ack       -> {reply, [#sio_message{type = ack, data = hive_socketio_parser:encode_id(Ack)}, Trigger], State}
    end;

echo(Args, _Event, Trigger, State) ->
    case Trigger#sio_message.id of
        undefined -> {reply, #sio_message{type = event, data = jsonx:encode(Args)}, State};
        Ack       -> {reply, [#sio_message{type = ack, data = hive_socketio_parser:encode_id(Ack)},
                              #sio_message{type = event, data = jsonx:encode(Args)}], State}
    end.

