-module(hive_error_utils).
-author('kajetan.rzepecki@zadane.pl').

-export([make_reply/1, make_json/1, make_json/2, format/1, format/2]).
-export([prettify/3, to_json/1]).

-include("hive_socketio.hrl").

-define(DEFAULT_TRUNCATION_LIMIT, 4096).
-define(DEFAULT_ERROR_EVENT, hive_error).

%% External funcitions:
make_reply({shutdown, Error}) ->
    make_reply(Error);

make_reply(Error) ->
    make_reply(hive_config:get(hive_build, development), Error).

make_reply(production, _Error) ->
    Data = jsonx:encode({[{name, ?DEFAULT_ERROR_EVENT},
                          {args, [{[{error, internal_error},
                                    {description, <<"Internal server error.">>}]}]}]}),
    #sio_message{type = event, data = Data};

make_reply(development, {Code, Description}) ->
    Data = jsonx:encode({[{name, ?DEFAULT_ERROR_EVENT},
                          {args, [{[{error, Code},
                                    {description, Description}]}]}]}),
    #sio_message{type = event, data = Data};

make_reply(development, Error) ->
    Data = jsonx:encode({[{name, ?DEFAULT_ERROR_EVENT},
                          {args, [{[{error, internal_error},
                                    {description, Error}]}]}]}),
    #sio_message{type = event, data = Data}.

format(Msg) ->
    format(Msg, []).

format(Format, Values) ->
    to_binary(lists:flatten(lager_trunc_io:format(Format, Values, ?DEFAULT_TRUNCATION_LIMIT))).

make_json(Description) ->
    make_json(bad_request, Description).

make_json(Code, Description) ->
    jsonx:encode({[{error, Code}, {description, Description}]}).

to_json(Value) ->
    jsonx:encode(Value).

prettify(Section, Errors, Separator) ->
    to_binary(prettify_iter(Section, Errors, Separator)).

%% Internal functions:
prettify_iter(_Section, [{<<"">>, Error}], _Separator) ->
    lists:flatten(io_lib:format("~s", [pretty_error(Error)]));

prettify_iter(Section, [{Field, Error}], _Separator) ->
    lists:flatten(io_lib:format("field \"~s\": ~s", [pretty_field(Section, Field), pretty_error(Error)]));

prettify_iter(Section, [{<<"">>, Error} | Errors], Separator) ->
    lists:flatten(io_lib:format("~s~s", [pretty_error(Error), Separator])) ++ prettify(Section, Errors, Separator);

prettify_iter(Section, [{Field, Error} | Errors], Separator) ->
    lists:flatten(io_lib:format("field \"~s\": ~s~s", [pretty_field(Section, Field), pretty_error(Error), Separator]))
        ++ prettify(Section, Errors, Separator).

pretty_error({invalid_json, File}) ->
    lists:flatten(io_lib:format("the \"~s\" file is not a valid JSON file", [to_string(File)]));

pretty_error({data_invalid, _Schema, wrong_length, Value}) ->
    lists:flatten(io_lib:format("value \"~s\" is of a wrong length", [to_string(Value)]));

pretty_error({data_invalid, _Schema, {not_unique, Value}, Values}) ->
    lists:flatten(io_lib:format("value \"~s\" is not unique in ~s", [to_string(Value), to_string(Values)]));

pretty_error({data_invalid, _Schema, {missing_required_property, Value}, _Values}) ->
    lists:flatten(io_lib:format("required property \"~s\" is missing", [to_string(Value)]));

pretty_error({data_invalid, _Schema, wrong_type, Value}) ->
    lists:flatten(io_lib:format("value \"~s\" is of a wrong type", [to_string(Value)]));

pretty_error({data_invalid, _Schema, not_in_range, Value}) ->
    lists:flatten(io_lib:format("value \"~s\" is outside of accepted range", [to_string(Value)]));

pretty_error(Error) ->
    lists:flatten(io_lib:format("~p", [Error])).

pretty_field(Section, <<"">>) ->
    to_string(Section);

pretty_field(<<"">>, Field) ->
    Field;

pretty_field(Section, Field) ->
    F = <<Section/binary, ".", Field/binary>>,
    to_string(F).

to_string(Str = [Int | _Rest]) when is_integer(Int) ->
    Str;

to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);

to_string(Value) ->
    binary_to_list(to_json(Value)).

to_binary(Value) when is_binary(Value) ->
    Value;

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);

to_binary(Value) ->
    format("~p", [Value]).
