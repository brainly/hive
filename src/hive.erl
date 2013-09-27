-module(hive).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(application).

-export([start/2, stop/1]).
-export([start/1, stop/0]).
-export([uptime/0, memory/0, processes/0]).

start([Build, ConfigFile | _Ignored]) ->
    %% The prelude definitions:
    hive_config:set(hive_build, Build), %% NOTE It's not really a build since it's run-time, but hey...
    hive_config:set(hive_config_file, list_to_binary(atom_to_list(ConfigFile))),
    hive_config:set(hive_schema_dir, <<"etc/schema">>),
    hive_config:set(hive_plugins_dir, <<"plugins">>),
    application:start(crypto),
    inets:start(),
    ibrowse:start(),
    application:start(ranch),
    application:start(cowboy),
    application:start(poolboy),
    application:start(hive).

start(_StartType, _StartArgs) ->
    lager:notice("Starting Hive..."),
    Ret = hive_root_sup:start_link(),
    lager:notice("Hive started!"),
    Ret.

stop() ->
    lager:notice("Stopping Hive..."),
    application:stop(hive),
    application:stop(poolboy),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto),
    inets:stop(),
    ibrowse:stop(),
    application:stop(lager),
    application:stop(sasl),
    lager:notice("Hive stopped!"),
    halt(0).

stop(Time) when is_integer(Time) ->
    %% NOTE We usually want to terminate gracefully, so we give us some time
    %% NOTE before we terminated all the applications.
    lager:notice("Hive will shutdown in ~p ms...", [Time]),
    hive_router:terminate_after(Time, shutdown),
    timer:apply_after(Time + 100, ?MODULE, stop, []);

stop(_State) ->
    ok.

%% Some hive statistics wrappers:
uptime() ->
    element(1, erlang:statistics(wall_clock)).

memory() ->
    erlang:memory().

processes() ->
    length(erlang:processes()).

