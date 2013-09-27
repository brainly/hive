-module(hive_hooks_utils).
-author('kajetan.rzepecki@zadane.pl').

-export([init_hooks/1, run_hooks/3, remove_hooks/2, add_hooks/2, state_json/2]).

-include("hive_client_handler.hrl").
-import(hive_client_utils, [combine_replies/2]).

-include("hive_socketio.hrl").
-import(hive_client_state_manager, [get_value/2]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, inc/2]).

%% External functions:
init_hooks([{Event, Hooks} | Rest]) ->
    case init_hooks_iter(Hooks) of
        {ok, NewHooks}  -> case init_hooks(Rest) of
                               {ok, RestHooks} -> {ok, dict:store(Event, NewHooks, RestHooks)};
                               {error, Error}  -> {error, Error}
                           end;
        {error, Error} -> {error, Error}
    end;

init_hooks([]) ->
    {ok, dict:new()}.

run_hooks(Event, Trigger, State) ->
    case dict:find(Event, State#state.hooks) of
        {ok, Hooks} -> run_hooks_iter(Event, Hooks, Trigger, {noreply, State});
        _           -> {noreply, State}
    end.

%% Internal functions:
init_hooks_iter([Hook | Hooks]) ->
    case init_hook(Hook) of
        {ok, NewHook}   -> case init_hooks_iter(Hooks) of
                               {ok, RestHooks} -> {ok, [NewHook | RestHooks]};
                               {error, Error}  -> {error, Error}
                           end;
        {error, Error} -> {error, Error}
    end;

init_hooks_iter([]) ->
    {ok, []}.

init_hook(Hook) ->
    Name = proplists:get_value(<<"hook">>, Hook),
    Args = proplists:get_value(<<"args">>, Hook),
    case hive_plugins:get(Name) of
        {ok, Init} ->
            case Init(Args) of
                {ok, Fun}      -> {ok, [proplists:property(function, Fun) | Hook]};
                {error, Error} -> {error, Error}
            end;

        {error, Error} ->
            {error, Error}
    end.

run_hooks_iter(_Event, [], _Trigger, Acc) ->
    Acc;

run_hooks_iter(Event, [Hook | Hooks], Trigger, {noreply, State}) ->
    run_hooks_iter(Event, Hooks, Trigger, run_hook(Event, Hook, Trigger, State));

run_hooks_iter(_Event, _Hooks, _Trigger, {stop, Error, State}) ->
    {stop, Error, State};

run_hooks_iter(_Event, _Hooks, _Trigger, {error, Error, State}) ->
    {error, Error, State};

run_hooks_iter(Event, [Hook | Hooks], Trigger, {reply, Replies, State}) ->
    case run_hook(Event, Hook, Trigger, State) of
        {noreply, NewState} ->
            run_hooks_iter(Event, Hooks, Trigger, {reply, Replies, NewState});

        {reply, Reply, NewState} ->
            run_hooks_iter(Event, Hooks, Trigger, {reply, combine_replies(Reply, Replies), NewState});

        {error, Error, NewState} ->
            {error, Error, NewState};

        {stop, Reason, NewState} ->
            {stop, Reason, NewState}
    end.

run_hook(Event, Hook, Trigger, State) ->
    Args = proplists:get_value(<<"args">>, Hook),
    Fun = proplists:get_value(function, Hook),
    %% NOTE Since there may be several hooks associated with each event,
    %% NOTE we need to increment the counters for each hook that is run.
    inc(?HOOK_EVENT_CALLS, Event),
    inc(?HOOK_CALLS),
    Fun(Args, Event, Trigger, State).

remove_hooks(Events, State) ->
    {ok, remove_hooks_iter(Events, State)}.

remove_hooks_iter([], State) ->
    State;

remove_hooks_iter([Event | Events], State) ->
    remove_hook(Event, remove_hooks_iter(Events, State)).

remove_hook(Event, State) ->
    NewHooks = dict:erase(Event, State#state.hooks),
    State#state{hooks = NewHooks}.

add_hooks(Hooks, State) ->
    case lists:foldl(fun(_, {error, Error}) ->
                             {errer, Error};

                        ({_Name, HookDescriptors}, ok) ->
                             lists:foldl(fun(_, {error, Error}) ->
                                                 {error, Error};

                                            (Descriptor, ok) ->
                                                 case hive_config:validate(<<"hook">>, Descriptor) of
                                                     {ok, _Hook} ->
                                                         ok;

                                                     {error, Error} ->
                                                         {error, Error}
                                                 end
                                         end,
                                         ok,
                                         HookDescriptors)
                     end,
                     ok,
                     Hooks)
    of
        ok ->
            case init_hooks(Hooks) of
                {error, Error}   -> {error, Error};
                {ok, InitdHooks} -> NewHooks = dict:merge(fun append_hooks/3,
                                                          InitdHooks,
                                                          State#state.hooks),
                                    {ok, State#state{hooks = NewHooks}}
            end;

        {error, Error} ->
            {error, Error}
    end.

append_hooks(_Key, A, B) ->
    lists:append(A, B).

state_json(Trigger, State) ->
    case get_value(all, State) of
        {ok, Data, NewState} ->
            Sid = State#state.sid,
            T = jsonx:decode(Trigger#sio_message.data),
            {ok, jsonx:encode({[{sid, Sid}, {trigger, T}, {state, Data}]}), NewState};

        {error, Error, NewState} ->
            {error, Error, NewState};

        {stop, Reason, NewState} ->
            {stop, Reason, NewState}
    end.
