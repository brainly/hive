-module(hive_cluster).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([connected_nodes/0, call/2, call/3, cast/2]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

%% Gen Server callbacks:
start_link() ->
    application:start(folsom),
    lager:notice("Starting Hive Cluster Manager..."),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {error, Error} -> hive_monitor:inc(?HIVE_ERRORS),
                          ErrorMsg = hive_error_utils:format("Cannot start Hive Cluster Manager: ~p", [Error]),
                          lager:error(ErrorMsg),
                          {error, {hive_error, ErrorMsg}};
        Ret            -> lager:notice("Hive Cluster Manager started!"),
                          Ret
    end.

terminate(_Reason, _State) ->
    ok.

init([]) ->
    hive_config:set(kernel, inet_dist_listen_min, hive_config:get(<<"hive.cluster_port_min">>, 9100)),
    hive_config:set(kernel, inet_dist_listen_max, hive_config:get(<<"hive.cluster_port_max">>, 9105)),
    net_kernel:start([list_to_atom(binary_to_list(hive_config:get(<<"hive.name">>, <<"hive">>))), longnames]),
    erlang:set_cookie(node(), list_to_atom(binary_to_list(hive_config:get(<<"hive.cluster_name">>, <<"hive_cluster">>)))),
    lists:map(fun(Node) ->
                      net_kernel:connect_node(list_to_atom(binary_to_list(Node)))
              end,
              hive_config:get(<<"hive.cluster_nodes">>, [])),
    {ok, undefined}.

%% External API:

%% And some utility functions:

call(Module, Message) ->
    call(Module, Message, infinity).

call(Module, Message, Timeout) ->
    inc(?CLUSTER_CALLS),
    get_reply(gen_server:multi_call(connected_nodes(), Module, Message, Timeout)).

cast(Module, Message) ->
    inc(?CLUSTER_CASTS),
    gen_server:abcast(Module, Message),
    ok.

connected_nodes() ->
    [node() | nodes()].

%% Gen Server handlers:
handle_call(Msg, _From, State) ->
    hive_monitor:inc(?CLUSTER_ERRORS),
    lager:warning("Unhandled Hive Cluster Manager call: ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    hive_monitor:inc(?CLUSTER_ERRORS),
    lager:warning("Unhandled Hive Cluster Manager cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    hive_monitor:inc(?CLUSTER_ERRORS),
    lager:warning("Unhandled Hive Cluster Manager info: ~p.", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    hive_monitor:inc(?CLUSTER_ERRORS),
    lager:warning("Unhandled Hive Cluster Manager code change."),
    {ok, State}.

%% Internal functions:
get_reply({[], _BadNodes}) ->
    hive_monitor:inc(?CLUSTER_ERRORS),
    ErrorMsg = hive_error_utils:format("Hive Cluster has halted and burns!"),
    lager:error(ErrorMsg),
    {error, {hive_cluster_error, ErrorMsg}};

get_reply({Replies, _BadNodes}) ->
    select_reply(lists:map(fun({_Node, Reply}) -> Reply end, Replies)).

select_reply([Reply]) ->
    %% NOTE If no replies were a sucess so far we have to return whatever is left.
    Reply;

select_reply([{error, _Reply} | Rest]) ->
    select_reply(Rest);

select_reply([ok | _Rest]) ->
    ok;

select_reply([{ok, Reply}| _Rest]) ->
    {ok, Reply}.
