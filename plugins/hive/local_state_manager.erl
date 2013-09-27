-module(local_state_manager).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(hive_plugin).

-export([load/0, unload/1, validate/2]).

-include("hive_client_handler.hrl").

%% External Functions
load() ->
    {ok, [{<<"sm.local">>, fun init/1}], undefined}.

unload(_State) ->
    ok.

validate(<<"sm.local">>, _StateManager) ->
    ok.

%% Internal functions:
init(State) ->
    InitialState = State#state.real_state,
    case init_state(InitialState) of
        {ok, RealState} ->
            {ok, State#state{
                   getter = fun get/2,
                   setter = fun set/3,
                   cleanup = fun cleanup/1,
                   real_state = RealState
                  }};

        Other ->
            Other
    end.

init_state(List = [{_Key, _Value} | _Rest]) ->
    {ok, dict:from_list(List)};

init_state(Value) ->
    {ok, dict:store(<<"initial_value">>, Value, dict:new())}.

get(all, State) ->
    {ok, dict:to_list(State#state.real_state), State};

get(What, State) ->
    {ok, dict:fetch(What, State#state.real_state), State}.

set(all, [{Key, Value} | Rest], State) ->
    {ok, _Ignored, NewState} = set(Key, Value, State),
    set(all, Rest, NewState);

set(all, [], State) ->
    {ok, [], State};

set(What, Value, State) ->
    {ok, Value, State#state{real_state = dict:store(What, Value, State#state.real_state)}}.

cleanup(State) ->
    State#state{real_state = undefined}.
