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
    {reply, Trigger, State};

echo(Args, _Event, _Trigger, State) ->
    {reply, #sio_message{type = event, data = jsonx:encode(Args)}, State}.
