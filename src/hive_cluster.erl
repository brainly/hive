-module(hive_cluster).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([connected_nodes/0]).

-include("hive_monitor.hrl").

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
connected_nodes() ->
    [node() | nodes()].

%% Gen Server handlers:
handle_call(Msg, _From, State) ->
    hive_monitor:inc(?HIVE_CLUSTER_ERRORS),
    lager:warning("Unhandled Hive Cluster Manager call: ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    hive_monitor:inc(?HIVE_CLUSTER_ERRORS),
    lager:warning("Unhandled Hive Cluster Manager cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    hive_monitor:inc(?HIVE_CLUSTER_ERRORS),
    lager:warning("Unhandled Hive Cluster Manager info: ~p.", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    hive_monitor:inc(?HIVE_CLUSTER_ERRORS),
    lager:warning("Unhandled Hive Cluster Manager code change."),
    {ok, State}.

%% Internal functions:
