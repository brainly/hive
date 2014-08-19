-module(hive_pubsub_utils).
-author('kajetan.rzepecki@brainly.com').

-export([make_cids/1]).

make_cids(Cid) when is_binary(Cid) ->
    [Cid];

make_cids(Cid) when is_integer(Cid) ->
    [integer_to_binary(Cid)];

make_cids([]) ->
    [];

make_cids([Prefix | Rest]) when is_binary(Prefix) ->
    lists:flatten([Prefix | make_cids(Rest)]);

make_cids([Prefix | Rest]) when is_binary(Prefix) ->
    lists:flatten([integer_to_binary(Prefix) | make_cids(Rest)]);

make_cids([{Prefix, Ids} | Rest]) ->
    lists:flatten([make_cids(Prefix, Ids) | make_cids(Rest)]);

make_cids([Cid | Rest]) when is_binary(Cid) ->
    lists:flatten([Cid | make_cids(Rest)]).

make_cids(_Prefix, []) ->
    [];

make_cids(Prefix, Types = [{_Type, _Ids} | _]) ->
    SubjectIds = proplists:get_value(<<"subjects">>, Types, []),
    SubjectIds2 = [integer_to_binary(Id) || Id <- SubjectIds],

    GradeIds   = proplists:get_value(<<"grades">>, Types, []),
    GradeIds2  = [integer_to_binary(Id) || Id <- GradeIds],

    lager:debug("SIDS: ~p, GIDS: ~p", [SubjectIds2, GradeIds2]),

    lists:map(fun(SubId) ->
                      Part1 = <<Prefix/binary, ".", SubId/binary>>,
                      [<<Part1/binary, ".", GradeId/binary>> || GradeId <- GradeIds2]
              end,
              SubjectIds2);

make_cids(Prefix, [Id | Ids]) ->
    lists:flatten([make_cids(Prefix, Id) | make_cids(Prefix, Ids)]);

make_cids(Prefix, Id) when is_integer(Id) ->
    IdBin = integer_to_binary(Id),
    [<<Prefix/binary, ".", IdBin/binary>>];

make_cids(Prefix, Id) when is_binary(Id) ->
    [<<Prefix/binary, ".", Id/binary>>].

