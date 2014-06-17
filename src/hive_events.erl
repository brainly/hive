-module(hive_events).
-author('kajetan.rzepecki@zadane.pl').

-export([init/0, init/1, dispatch/2, parse/1, new/1]).

-include("hive_events.hrl").
-include("hive_client_handler.hrl").
-import(hive_client_utils, [combine_replies/2]).

%% External functions:
init() ->
    init(hive_config:get(<<"clients.actions">>)).

init(Handlers) ->
    case init_event_handlers(Handlers, dict:new()) of
        {error, Error} -> %%inc(?HOOK_ERRORS), %% FIXME Event Errors?
            {error, Error};
        Other          -> Other
    end.

parse(Data) ->
    case jsonx:decode(Data, [{format, proplist}]) of
        {error, Code, _} ->
            ErrorMsg = hive_error_utils:format("Bad internal event: ~s.", [Data]),
            lager:warning(ErrorMsg),
            {error, {Code, ErrorMsg}};

        JSON = [Object | _Objects] when is_list(Object) ->
            lists:foldl(fun(_, {error, Error}) ->
                                {error, Error};

                           (Event, {ok, Acc}) ->
                                case new(Event) of
                                    {ok, E}   -> {ok, [E | Acc]};
                                    Otherwise -> Otherwise
                                end
                        end,
                        {ok, []},
                        JSON);

        JSON ->
            new(JSON)
    end.

new(JSON) ->
    case hive_config:validate(<<"internal_event">>, JSON) of
        {ok, Event}    -> Action = proplists:get_value(<<"action">>, Event),
                          Args = proplists:get_value(<<"args">>, Event),
                          Id = proplists:get_value(<<"trigger_id">>, Event, <<"">>),
                          %% NOTE We encode the args in order to avoid copying large structures
                          %% NOTE between Hive processes.
                          {ok, #internal_event{
                                  action = Action,
                                  id = Id,
                                  args = jsonx:encode(Args)
                                 }};
        {error, Error} -> {error, Error}
    end.

dispatch(Events, State) when is_list(Events) ->
    dispatch_iter(Events, {noreply, State});

dispatch(InternalEvent = #internal_event{}, State) ->
    Action = InternalEvent#internal_event.action,
    case dict:find(Action, State#state.event_handlers) of
        {ok, Handler} ->
            %% FIXME Add common dispatcher config and pass it here.
            Args = proplists:get_value(<<"args">>, Handler),
            Fun = proplists:get_value(function, Handler),
            Fun(Args, Action, InternalEvent, State);

        error ->
            ErrorMsg = hive_error_utils:format("Unhandled Hive Internal Event: ~p", [InternalEvent]),
            lager:warning(ErrorMsg),
            {error, {bad_internal_event, ErrorMsg}, State}
    end;

dispatch(Event, State) ->
    ErrorMsg = hive_error_utils:format("Unhandled Hive Hooks Client internal event: ~p", [Event]),
    lager:warning(ErrorMsg),
    {error, {bad_internal_event, ErrorMsg}, State}.

dispatch_iter([], Acc) ->
    Acc;

dispatch_iter([Event | Events], {noreply, State}) ->
    dispatch_iter(Events, dispatch(Event, State));

dispatch_iter([Event | Events], {reply, Replies, State}) ->
    case dispatch(Event, State) of
        {noreply, NewState} ->
            dispatch_iter(Events, {reply, Replies, NewState});

        {reply, Reply, NewState} ->
            dispatch_iter(Events, {reply, combine_replies(Reply, Replies), NewState});

        {error, Error, NewState} ->
            {error, Error, NewState};

        {stop, Reason, NewState} ->
            {stop, Reason, NewState}
    end;

dispatch_iter(_Events, {stop, Error, State}) ->
    {stop, Error, State};

dispatch_iter(_Events, {error, Error, State}) ->
    {error, Error, State}.

%% Internal functions:
init_event_handlers(Handlers, Dict) ->
    lists:foldl(fun(_, {error, Error}) ->
                        {error, Error};

                   ({Name, HandlerDescriptors}, {ok, NewDict}) ->
                        lists:foldl(fun(_, {error, Error}) ->
                                            {error, Error};

                                       (Handler, {ok, NewestDict}) ->
                                            case init_handler(Handler) of
                                                {ok, NewHandler} ->
                                                    %% FIXME Allow this to store several actions at a given name.
                                                    %% FIXME Merge this with the Hive Hooks.
                                                    {ok, dict:store(Name, NewHandler, NewestDict)};

                                                {error, Error} ->
                                                    {error, Error}
                                            end
                                    end,
                                    {ok, NewDict},
                                    HandlerDescriptors)
                end,
                {ok, Dict},
                Handlers).

init_handler(Handler) ->
    Action = proplists:get_value(<<"action">>, Handler),
    Args = proplists:get_value(<<"args">>, Handler),
    case hive_plugins:get(Action) of
        {ok, Init} ->
            case Init(Args) of
                {ok, Fun}      -> {ok, [proplists:property(function, Fun) | Handler]};
                {error, Error} -> {error, Error}
            end;

        {error, Error} ->
            {error, Error}
    end.
