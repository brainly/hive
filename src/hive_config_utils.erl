-module(hive_config_utils).
-author('kajetan.rzepecki@zadane.pl').

-export([basename/1, basenames/1, load_dir/1, is_dir/1]).

-include_lib("kernel/include/file.hrl").

basenames([]) ->
    [];

basenames([File | Files]) ->
    [basename(File) | basenames(Files)].

basename(File) ->
    case filename:rootname(filename:basename(File)) of
        Name when is_binary(Name) -> binary_to_list(Name);
        Otherwise                 -> Otherwise
    end.

load_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foldl(fun(_, {error, Error}) ->
                                {error, Error};

                           (File, {ok, SomeFiles}) ->
                                FileName = filename:join(Dir, File),
                                case is_dir(FileName) of
                                    true ->
                                        case load_dir(FileName) of
                                            {ok, MoreFiles} ->
                                                {ok, MoreFiles ++ SomeFiles};

                                            {error, Error} ->
                                                {error, Error}
                                        end;

                                    false ->
                                        {ok, [FileName | SomeFiles]};

                                    {error, Error} ->
                                        {error, Error}
                                end
                        end,
                        {ok, []},
                        Files);

        {error, Error} ->
            {error, Error}
    end.

is_dir(File) ->
    case file:read_file_info(File) of
        {ok, Info} ->
            case Info#file_info.type of
                directory -> true;
                _         -> false
            end;

        {error, Error} ->
            {error, Error}
    end.
