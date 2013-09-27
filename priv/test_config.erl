#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/jsonx/ebin  deps/jesse/ebin deps/lager/ebin deps/goldrush/ebin deps/folsom/ebin -sname test_config

-module(test_config).
%% An utility to test and validate Hive Server configuration files.

-define(log(Level, Line, Args), io:format("[~p] ~s\n", [Level, lists:flatten(io_lib:format(Line, Args))])).
-define(log(Level, Line), io:format("[~p] ~s\n", [Level, Line])).

main([]) ->
    io:format("Usage: ./test_config.erl config_file.json\n"),
    halt(1);

main([Filename]) ->
    %%application:start(lager),
    hive_config:set(hive_config_file, list_to_binary(Filename)),
    hive_config:set(hive_schema_dir, <<"etc/schema">>),
    hive_config:set(hive_plugins_dir, <<"plugins">>),
    erlang:process_flag(trap_exit, true),
    case hive_env_sup:start_link() of
        {ok, _} ->
            ?log(info, "Config will work properly."),
            stop(0);

        {error, {_Code, Error}} ->
            ?log(error, "Config won't work properly: ~s", [Error]),
            stop(1)
    end.

stop(Ret) ->
    halt(Ret).
