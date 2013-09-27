-module(hive_client_state_manager).
-author('kajetan.rzepecki@zadane.pl').

-export([init_state/2, get_value/2, set_value/3, cleanup/1]).

-include("hive_client_handler.hrl").
-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

%% External functions:
init_state(Sid, StateDescription) ->
    inc(?STATE_MGR_REQUESTS),
    inc(?STATE_MGR_INITS),
    StateManager = proplists:get_value(<<"state_manager">>, StateDescription),
    InitialState = proplists:get_value(<<"initial_value">>, StateDescription),
    Args = proplists:get_value(<<"args">>, StateDescription),
    case hive_plugins:get(StateManager) of
        {ok, Init} ->
            case Init(#state{
                         sid = Sid,
                         real_state = InitialState,
                         args = Args
                        })
            of
                {ok, State} ->
                    {ok, State};

                {stop, Reason} ->
                    lager:debug("Hive Client State Manager encountered an error: ~p", [Reason]),
                    {stop, Reason}
            end;

        {error, Error} ->
            lager:debug("Hive Client State Manager encountered an error: ~p", [Error]),
            {stop, Error}
    end.

get_value(What, State) ->
    inc(?STATE_MGR_REQUESTS),
    inc(?STATE_MGR_GETS),
    Getter = State#state.getter,
    Getter(What, State).

set_value(What, Value, State) ->
    inc(?STATE_MGR_REQUESTS),
    inc(?STATE_MGR_SETS),
    Setter = State#state.setter,
    Setter(What, Value, State).

cleanup(State) ->
    inc(?STATE_MGR_REQUESTS),
    inc(?STATE_MGR_CLEANUPS),
    Cleanup = State#state.cleanup,
    Cleanup(State).

