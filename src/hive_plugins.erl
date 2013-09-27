-module(hive_plugins).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([load/1,  get/1, validate/2]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-record(state, {
          modules,        %% Plugin modules.
          plugins         %% Plugins themselves.
         }).

%% Gen Server related
start_link() ->
    lager:notice("Starting Hive Plugins Manager..."),
    Dir = hive_config:get(hive_plugins_dir),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, Dir, []) of
        {error, Error} -> lager:debug("Hive Plugins Manager encountered an error: ~p.", [Error]),
                          {error, Error};
        Ret            -> lager:notice("Hive Plugins Manager started!"),
                          Ret
    end.

init(Dir) ->
    case load(Dir, #state{
                      plugins = dict:new(),
                      modules = []
                     })
    of
        {ok, NewState} -> {ok, NewState};
        {error, Error} -> {stop, Error}
    end.

terminate(_Reason, State) ->
    lists:foreach(fun({Module, PluginState}) ->
                          Module:unload(PluginState)
                  end,
                  State#state.modules).

%% External functions:
load(DirName) ->
    gen_server:call(?MODULE, {load_dir, DirName}).

validate(Name, Config) ->
    gen_server:call(?MODULE, {validate, Name, Config}).

get(Name) ->
    gen_server:call(?MODULE, {get_plugin, Name}).

%% Gen Server handlers:
handle_call({load_dir, DirName}, _From, State) ->
    case load(DirName, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Error} -> {reply, {error, Error}, State}
    end;

handle_call({get_plugin, Name}, _From, State) ->
    {reply, get_plugin(Name, State), State};

handle_call({validate, Name, Config}, _From, State) ->
    case get_plugin_module(Name, State) of
        {ok, Module}   -> {reply, Module:validate(Name, Config), State};
        {error, Error} -> {reply, {error, Error}, State}
    end;

handle_call(Msg, _From, State) ->
    inc(?HIVE_PLUGIN_ERRORS),
    lager:warning("Unhandled Hive Plugins Manager call: ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    inc(?HIVE_PLUGIN_ERRORS),
    lager:warning("Unhandled Hive Plugins Manager cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    inc(?HIVE_PLUGIN_ERRORS),
    lager:warning("Unhandled Hive Plugins Manager info: ~p.", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    inc(?HIVE_PLUGIN_ERRORS),
    lager:warning("Unhandled Hive Plugins Manager code change."),
    {ok, State}.

%% Internal functions:
load(DirName, State) ->
    case hive_config_utils:load_dir(DirName) of
        {error, Error} ->
            inc(?HIVE_PLUGIN_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive Plugins Manager was unable to load plugins directory ~s: ~p",
                                                [DirName, Error]),
            lager:error(ErrorMsg),
            {error, {plugins_error, ErrorMsg}};

        {ok, Files} ->
            case select_plugins(Files) of
                {error, Error} ->
                    inc(?HIVE_PLUGIN_ERRORS),
                    ErrorMsg = hive_error_utils:format("Hive Plugins Manager was unable to load plugins directory ~s: ~p",
                                                        [DirName, Error]),
                    lager:error(ErrorMsg),
                    {error, {plugins_error, ErrorMsg}};

                {ok, Plugins} ->
                    case ensure_loaded(Plugins, State) of
                        {ok, NewState} -> {ok, NewState};
                        {error, Error} -> {error, Error}
                    end
            end
    end.

select_plugins(FileNames) ->
    select_plugins(hive_config_utils:basenames(FileNames), []).

select_plugins([], Plugins) ->
    {ok, Plugins};

select_plugins([File | Files], Plugins) ->
    case postfix(File) of
        undefined  -> select_plugins(Files, Plugins);
        _Otherwise -> select_plugins(Files, [File | Plugins])
    end.

ensure_loaded([], State) ->
    {ok, State};

ensure_loaded([Plugin | Plugins], State) ->
    case load_plugin(Plugin, State) of
        {ok, NewState} -> ensure_loaded(Plugins, NewState);
        {error, Error} -> {error, Error}
    end.

load_plugin(Plugin, State) ->
    PluginModule = list_to_atom(Plugin),
    case load_plugin_module(PluginModule) of
        {stop, Reason} ->
            inc(?HIVE_PLUGIN_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive Plugins Manager was unable to load plugin ~s: ~p",
                                                [Plugin, Reason]),
            lager:error(ErrorMsg),
            {error, {plugins_error, ErrorMsg}};

        {ok, PluginObjects, PluginState} ->
            {ok, update_plugins(PluginModule,
                                PluginState,
                                PluginObjects,
                                State)}
    end.

load_plugin_module(Module) ->
    lager:notice("Loading Hive Plugin: ~p", [Module]),
    case code:ensure_loaded(Module) of
        {error, Error} ->
            {stop, Error};

        {module, Module} ->
            Module:load()
    end.

postfix(File) ->
    case re:run(File, <<"^(.*)_(hook|event|state_manager|connector)s?$">>, [{capture, [2], binary}]) of
        {match, [Postfix]} -> Postfix;
        nomatch            -> undefined
    end.

update_plugins(PluginModule, PluginState, PluginObjects, State) ->
    Objs = lists:map(fun({Name, Value}) ->
                             {Name, {Value, PluginModule}}
                     end,
                     PluginObjects),
    set_modules([{PluginModule, PluginState} | State#state.modules],
                set_plugins(dict:merge(fun(_Key, A, _B) -> A end,
                                       dict:from_list(Objs),
                                       State#state.plugins),
                            State)).

get_plugin(Name, State) ->
    case dict:find(Name, State#state.plugins) of
        {ok, {Value, _Module}} ->
            {ok, Value};

        error ->
            inc(?HIVE_PLUGIN_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive Plugin ~s isn't loaded!", [Name]),
            lager:error(ErrorMsg),
            {error, {plugins_error, ErrorMsg}}
    end.

get_plugin_module(Name, State) ->
    case dict:find(Name, State#state.plugins) of
        {ok, {_Value, Module}} ->
            {ok, Module};

        error ->
            inc(?HIVE_PLUGIN_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive Plugin ~s isn't loaded!", [Name]),
            lager:error(ErrorMsg),
            {error, {plugins_error, ErrorMsg}}
    end.

get_plugin_state(Name, State) ->
    case get_plugin_module(Name, State) of
        {ok, Module} ->
            case proplists:get_value(Module, State#state.modules) of
                undefined ->
                    inc(?HIVE_PLUGIN_ERRORS),
                    ErrorMsg = hive_error_utils:format("Hive Plugin ~s isn't loaded!", [Name]),
                    lager:error(ErrorMsg),
                    {error, {plugins_error, ErrorMsg}};

                PState ->
                    {ok, PState}
            end;

        {error, Error} ->
            {error, Error}
    end.

set_plugins(Plugins, State) ->
    State#state{plugins = Plugins}.

set_modules(Modules, State) ->
    State#state{modules = Modules}.
