-module(hive_config).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([set/2, get/1, get/2, add_schema/2, load_schema/1, load_schemas/1, connector/1, validate/2]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

-import(hive_error_utils, [format/2, prettify/3, to_json/1]).

%% Gen Server callbacks:
start_link() ->
    lager:notice("Starting Hive Config Manager..."),
    SchemaDir = hive_config:get(hive_schema_dir),
    ConfigFile = hive_config:get(hive_config_file),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfigFile, SchemaDir], []) of
        {error, Error} -> lager:debug("Hive Config Manager encountered an error: ~p.", [Error]),
                          {error, Error};
        Ret            -> lager:notice("Hive Config Manager started!"),
                          Ret
    end.

init([ConfigFile, SchemaDir]) ->
    case load_jsonschemas(SchemaDir) of
        ok ->
            case load_config(ConfigFile) of
                ok ->
                    case check_integrity(ConfigFile) of
                        ok ->
                            {ok, undefined};

                        {error, Error} ->
                            {stop, Error}
                    end;

                {error, Error} ->
                    {stop, Error}
            end;

        {error, Error} ->
            {stop, Error}
    end.

terminate(_Reason, _State) ->
    ok.

%% External functions:
set(Name, Value) ->
    ok = application:set_env(hive, Name, Value).

get(Name) ->
    case application:get_env(hive, Name) of
        {ok, Value} -> Value;
        _           -> undefined
    end.

get(Name, Default) ->
    case hive_config:get(Name) of
        undefined -> Default;
        Value     -> Value
    end.

add_schema(Key, Schema) ->
    gen_server:call(?MODULE, {add_schema, Key, Schema}).

load_schema(Filename) ->
    gen_server:call(?MODULE, {load_schema, Filename}).

load_schemas(Path) ->
    gen_server:call(?MODULE, {load_schema, Path}).

connector(Name) ->
    %% FIXME This is ugly but functional. Needs fixing.
    case proplists:get_value(Name, hive_config:get(<<"connectors.pools">>)) of
        undefined -> {error, {no_config, Name}};
        Value     -> {ok, Value}
    end.

validate(Key, Value) ->
    gen_server:call(?MODULE, {validate, Key, Value}).

%% Gen Server handlers:
handle_call({load_schema, Path}, _From, State) ->
    case hive_config_utils:is_dir(Path) of
        true  -> {reply, load_jsonschemas(Path), State};
        false -> {reply, load_jsonschema(Path), State}
    end;

handle_call({validate, <<"internal_event">>, Event}, _From, State) ->
    case validate_section(<<"internal_event">>, <<"">>, Event) of
        {error, {_Code, Error}} ->
            ErrorMsg = format("Bad internal event ~s: ~s.", [to_json(Event), Error]),
            {reply, {error, {bad_internal_event, ErrorMsg}}, State};

        {ok, Value} ->
            {reply, {ok, Value}, State}
    end;

handle_call({validate, <<"external_event">>, Event}, _From, State) ->
    case validate_section(<<"external_event">>, <<"">>, Event) of
        {error, {_Code, Error}} ->
            ErrorMsg = format("Bad external event ~s: ~s.", [to_json(Event), Error]),
            {reply, {error, {bad_external_event, ErrorMsg}}, State};

        {ok, Value} ->
            {reply, {ok, Value}, State}
    end;

handle_call({validate, <<"connector">>, Connector}, _From, State) ->
    case validate_section(<<"connector">>, <<"">>, Connector) of
        {error, {_Code, Error}} ->
            ErrorMsg = format("Bad connector descriptor ~s: ~s.", [to_json(Connector), Error]),
            {reply, {error, {bad_connector_descriptor, ErrorMsg}}, State};

        {ok, Value} ->
            Plugin = proplists:get_value(<<"connector">>, Connector),
            case hive_plugins:validate(Plugin, Connector) of
                ok             -> {reply, {ok, Value}, State};
                {error, Error} -> {reply, {error, Error}, State}
            end
    end;

handle_call({validate, <<"hook">>, Hook}, _From, State) ->
    case validate_section(<<"hook">>, <<"">>, Hook) of
        {error, {_Code, Error}} ->
            ErrorMsg = format("Bad hook descriptor ~s: ~s.", [to_json(Hook), Error]),
            {reply, {error, {bad_connector_descriptor, ErrorMsg}}, State};

        {ok, Value} ->
            Plugin = proplists:get_value(<<"hook">>, Hook),
            case hive_plugins:validate(Plugin, Hook) of
                ok             -> {reply, {ok, Value}, State};
                {error, Error} -> {reply, {error, Error}, State}
            end
    end;

handle_call({validate, <<"action">>, Action}, _From, State) ->
    case validate_section(<<"action">>, <<"">>, Action) of
        {error, {_Code, Error}} ->
            ErrorMsg = format("Bad action descriptor ~s: ~s.", [to_json(Action), Error]),
            {reply, {error, {bad_connector_descriptor, ErrorMsg}}, State};

        {ok, Value} ->
            Plugin = proplists:get_value(<<"action">>, Action),
            case hive_plugins:validate(Plugin, Action) of
                ok             -> {reply, {ok, Value}, State};
                {error, Error} -> {reply, {error, Error}, State}
            end
    end;

handle_call({validate, Key, Value}, _From, State) ->
    case validate_section(Key, <<"">>, Value) of
        {error, {_Code, Error}} ->
            ErrorMsg = format("Bad ~s datum ~s: ~p.", [Key, to_json(Value), Error]),
            {reply, {error, {bad_datum, ErrorMsg}}, State};

        {ok, Value} ->
            {reply, {ok, Value}, State}
    end;

handle_call(Msg, _From, State) ->
    inc(?CONFIG_ERRORS),
    lager:warning("Unhandled Hive Config Manager call: ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    inc(?CONFIG_ERRORS),
    lager:warning("Unhandled Hive Config Manager cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    inc(?CONFIG_ERRORS),
    lager:warning("Unhandled Hive Config Manager info: ~p.", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    inc(?CONFIG_ERRORS),
    lager:warning("Unhandled Hive Config Manager code change."),
    {ok, State}.

%% Internal functions:
load_jsonschemas(Path) ->
    case hive_config_utils:load_dir(Path) of
        {error, Error} ->
            lager:error("Malformed or missing schemas ~s: ~p.", [Path, Error]),
            {error, Error};

        {ok, SchemaFiles} ->
            lists:foldl(fun(_, {error, Error}) ->
                                {error, Error};

                           (SchemaFile, ok) ->
                                load_jsonschema(SchemaFile)
                        end,
                        ok,
                        SchemaFiles)
    end.

load_config(FileName) ->
    case read(FileName) of
        {error, Error} ->
            {error, Error};

        {ok, Config} ->
            case validate_and_load(FileName, Config) of
                ok             -> ok;
                {error, Error} -> {error, Error}
            end
    end.

check_integrity(FileName) ->
    lists:foldl(fun(_, {error, Error}) ->
                        {error, Error};

                   ({Name, {Path, Config}}, ok) ->
                        case hive_plugins:validate(Name, Config) of
                            ok ->
                                ok;

                            {error, {Code, Error}} ->
                                inc(?CONFIG_ERRORS),
                                ErrorMsg = format("Config ~s integrity check failed: field \"~s\" - ~s.",
                                                  [FileName, Path, Error]),
                                lager:error(ErrorMsg),
                                {error, {Code, ErrorMsg}};

                            {error, Error} when is_binary(Error) ->
                                inc(?CONFIG_ERRORS),
                                ErrorMsg = format("Config ~s integrity check failed: field \"~s\" - ~s.",
                                                  [FileName, Path, Error]),
                                lager:error(ErrorMsg),
                                {error, {invalid_config, ErrorMsg}};

                            {error, Error} ->
                                inc(?CONFIG_ERRORS),
                                ErrorMsg = format("Config ~s integrity check failed: field \"~s\" - ~p.",
                                                  [FileName, Path, Error]),
                                lager:error(ErrorMsg),
                                {error, {invalid_config, ErrorMsg}}
                        end
                end,
                ok,
                build_mapping()).

load_jsonschema(FileName) ->
    case read(FileName) of
        {error, Error} ->
            {error, Error};

        {ok, Schema} ->
            lists:foldl(fun(_, {error, Error}) ->
                                {error, Error};

                           ({Key, Value}, ok) ->
                                set_schema(Key, Value)
                        end,
                        ok,
                        [{hive_config_utils:basename(FileName), Schema} | find_subschemas(Schema)])
    end.

read(FileName) ->
    case file:read_file(FileName) of
        {ok, Contents} ->
            case jsonx:decode(Contents) of
                {error, Code, Char} ->
                    inc(?CONFIG_ERRORS),
                    ErrorMsg = format("Error in file ~s at position ~p: ~p.", [FileName, Char, Code]),
                    lager:error(ErrorMsg),
                    {error, {Code, ErrorMsg}};

                JSON ->
                    {ok, JSON}
            end;

        {error, enoent} ->
            inc(?CONFIG_ERRORS),
            lager:error("File missing: ~s.", [FileName]),
            {error, {file_missing, FileName}};

        {error, Error} ->
            inc(?CONFIG_ERRORS),
            lager:error("File malformed: ~s.", [FileName]),
            {error, {file_error, Error}}
    end.

validate_and_load(FileName, Config) ->
    case validate_section(FileName, <<"config">>, <<"">>, Config) of
        {error, Error} ->
            {error, Error};

        {ok, {Sections}} ->
            case lists:foldl(validate_and_load(FileName), ok, Sections) of
                {error, Error} ->
                    {error, Error};

                ok->
                    ok
            end
    end.

validate_and_load(FileName) ->
    fun(_, {error, Error}) ->
            {error, Error};

       ({Section, Values}, ok) ->
            case validate_section(FileName, Section, Section, Values) of
                {ok, SecValues} -> load_section(Section, SecValues);
                {error, Error}  -> {error, Error}
            end
    end.

validate_section(FileName, Section, Prefix, Values) when is_binary(Values) ->
    SecFile = filename:join(filename:dirname(FileName), Values),
    case read(SecFile) of
        {ok, Contents} -> validate_section(FileName, Section, Prefix, Contents);
        {error, Error} -> {error, Error}
    end;

validate_section(FileName, Section, Prefix, Values) ->
    case validate_section(Section, Prefix, Values) of
        {ok, Value} ->
            {ok, Value};

        {error, {Code, Error}} ->
            inc(?CONFIG_ERRORS),
            ErrorMsg = format("Error in file ~s: ~s.", [FileName, Error]),
            lager:error(ErrorMsg),
            {error, {Code, ErrorMsg}}
    end.

validate_section(Section, Prefix, Values) ->
    case get_schema(Section) of
        {ok, Schema} ->
            case jesse:validate_with_accumulator(Schema, Values, fun acc_errors/3, []) of
                {ok, Value}     -> {ok, Value};
                {error, Errors} -> {error, {invalid_config, prettify(Prefix, Errors, ", ")}}
            end;

        {error, Error} ->
            {error, Error}
    end.

load_section(Section, {Values}) ->
    lists:foreach(fun({Property, Value}) ->
                          Prop = <<Section/binary, ".", Property/binary>>,
                          %% NOTE The rest of the code expects "proplistized" data.
                          hive_config:set(Prop, proplistize(Value))
                  end,
                  Values).

build_mapping() ->
    %% NOTE Each of these is a [{Name, {Path, Descriptor}}].
    build_connectors_mapping()
        ++ build_hooks_mapping()
        ++ build_events_mapping()
        ++ build_sm_mapping().

build_sm_mapping() ->
    State = hive_config:get(<<"clients.state">>),
    Manager = proplists:get_value(<<"state_manager">>, State),
    [{Manager, {<<"clients.state">>, State}}].

build_hooks_mapping() ->
    Mapping = lists:map(fun({Event, Hooks}) ->
                                lists:map(fun({Index, Descriptor}) ->
                                                  Id = integer_to_binary(Index),
                                                  Path = <<"clients.hooks.", Event/binary, "[", Id/binary, "]">>,
                                                  Hook = proplists:get_value(<<"hook">>, Descriptor),
                                                  {Hook, {Path, Descriptor}}
                                          end,
                                          lists:zip(lists:seq(0, length(Hooks)-1), Hooks))
                        end,
                        hive_config:get(<<"clients.hooks">>)),
    lists:sort(lists:flatten(Mapping)).

build_connectors_mapping() ->
    lists:sort(lists:map(fun({Name, Descriptor}) ->
                                 Path = <<"connectors.pools.", Name/binary>>,
                                 Connector = proplists:get_value(<<"connector">>, Descriptor),
                                 {Connector, {Path, Descriptor}}
                         end,
                         hive_config:get(<<"connectors.pools">>))).

build_events_mapping() ->
    Mapping = lists:map(fun({Action, Handlers}) ->
                                lists:map(fun({Index, Descriptor}) ->
                                                  Id = integer_to_binary(Index),
                                                  Path = <<"clients.actions.", Action/binary, "[", Id/binary, "]">>,
                                                  Handler = proplists:get_value(<<"action">>, Descriptor),
                                                  {Handler, {Path, Descriptor}}
                                          end,
                                          lists:zip(lists:seq(0, length(Handlers)-1), Handlers))
                        end,
                        hive_config:get(<<"clients.actions">>)),
    lists:sort(lists:flatten(Mapping)).

%% Helper functions:
acc_errors(Field, Error, List) ->
    [{format("~s", [Field]), Error} | List].

find_subschemas(JSON) ->
    find_subschemas(JSON, []).

find_subschemas({Values}, SubSchemas) ->
    case proplists:get_value(<<"id">>, Values) of
        undefined -> find_subschemas(Values, SubSchemas);
        Id        -> [{Id, {Values}} | SubSchemas]
    end;

find_subschemas([], SubSchemas) ->
    SubSchemas;

find_subschemas([{_Key, Value} | Values], SubSchemas) ->
    find_subschemas(Values, find_subschemas(Value, SubSchemas));

find_subschemas([Value | Values], SubSchemas) ->
    find_subschemas(Values, find_subschemas(Value, SubSchemas));

find_subschemas(_Value, SubSchemas) ->
    SubSchemas.

proplistize({Value}) ->
    proplistize(Value);

proplistize({Key, Value}) ->
    {Key, proplistize(Value)};

proplistize([Value | Values]) ->
    [proplistize(Value) | proplistize(Values)];

proplistize(Value) ->
    Value.

set_schema(Key, Schema) when is_binary(Key) ->
    jesse:add_schema(Key, Schema);

set_schema(Key, Schema) when is_list(Key) ->
    set_schema(format("~s", [Key]), Schema);

set_schema(Key, Schema) ->
    set_schema(format("~p", [Key]), Schema).

get_schema(Key) ->
    case catch jesse_database:read(Key) of
        {database_error, Key, Code} ->
            {error, {Code, Key}};

        Schema ->
            {ok, Schema}
    end.
