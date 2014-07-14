#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/jsonx/ebin deps/jesse/ebin deps/lager/ebin deps/folsom/ebin -sname prep_hive

-module(prep_hive).
%% An utility script that generates additional erl flags.

-define(LAGER_FILE, "/tmp/lager.config").

main([]) ->
    io:format("Usage: ./prep_hive path/to/config.json"),
    halt(1);

main([Plugins, Schema, Config]) ->
    application:start(lager),
    hive_config:set(hive_config_file, list_to_binary(Config)),
    hive_config:set(hive_schema_dir, list_to_binary(Schema)),
    hive_config:set(hive_plugins_dir, list_to_binary(Plugins)),
    erlang:process_flag(trap_exit, true),
    hive_env_sup:start_link(),
    setup_logging(),
    LogDir = hive_config:get(<<"log.dir">>),
    DumpFile = case binary:last(LogDir) of
                   $/ -> binary_to_list(LogDir) ++ "erl_crash.dump";
                   _  -> binary_to_list(LogDir) ++ "/erl_crash.dump"
               end,
    MaxProcesses = hive_config:get(<<"hive.max_processes">>, 262144),
    Value = io_lib:format("-config ~s +P ~p -env ERL_CRASH_DUMP ~s", [?LAGER_FILE, MaxProcesses, DumpFile]),
    io:format(Value ++ "\n"),
    halt(0).

setup_logging() ->
    DirBin = hive_config:get(<<"log.dir">>),
    Dir = case binary:last(DirBin) of
              $/ -> binary_to_list(DirBin);
              _  -> binary_to_list(DirBin) ++ "/"
          end,
    ConsoleLevel = to_atom(hive_config:get(<<"log.console_level">>, <<"debug">>)),
    Config = {lager, [{crash_log, Dir ++ "crash.log"},
                      {handlers,[{lager_console_backend, ConsoleLevel},
                                 {lager_file_backend, [{level, error}, {file, Dir ++ "error.log"}]},
                                 {lager_file_backend, [{level, ConsoleLevel}, {file, Dir ++ "console.log"}]},
                                 {lager_file_backend, [{level, to_atom(hive_config:get(<<"log.file_level">>, <<"debug">>))},
                                                       {file, Dir ++ "hive.log"}]}]}]},
    file:write_file(?LAGER_FILE, list_to_binary(io_lib:format("[~p].", [Config]))).

to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).
