-module(hive_server_wrapper).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {server_listener}).

%% This is a tiny wrapper that makes restarts of various Hive HTTP servers predictable and well-behaved.
%% Gen server callbacks:
start_link(Server) ->
    gen_server:start_link({local, Server}, ?MODULE, [Server], []).

init([Server]) ->
    case Server:start() of
        {ok, Listener} -> {ok, #state{server_listener = Listener}};
        {error, Error} -> {stop, Error}
    end.

terminate(_Reason, State) ->
    cowboy:stop_listener(State#state.server_listener).

%% Gen server handlers:
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
