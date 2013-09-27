-module(hive_socketio_parser_tests).
-author('kajetan.rzepecki@zadane.pl').

-include_lib("eunit/include/eunit.hrl").
-include("hive_socketio.hrl").

-import(hive_socketio_parser, [encode/1, encode_batch/1, split/1, decode/1, decode_batch/1, decode_maybe_batch/1]).

-define(M, 16#fffd/utf8).

encode_test_() ->
    lists:map(fun({Index, T}) ->
                      I = list_to_binary(integer_to_list(Index)),
                      ?_assert(encode(#sio_message{type = T}) =:= <<I/binary, ":::">>)
              end,
              lists:zip(lists:seq(0, length(?MESSAGE_TYPES)-1), ?MESSAGE_TYPES))
        ++
        [?_assert(encode(#sio_message{}) =:= ?NOOP),
         ?_assert(encode(#sio_message{type = ack}) =:= <<"6:::">>),
         ?_assert(encode(#sio_message{id = {server, 23}}) =:= <<"8:23::">>),
         ?_assert(encode(#sio_message{id = {client, 23}}) =:= <<"8:23+::">>),
         ?_assert(encode(#sio_message{endpoint = <<"foo">>}) =:= <<"8::foo:">>),
         ?_assert(encode(#sio_message{data = <<"foo">>}) =:= <<"8:::foo">>),
         ?_assert(encode(#sio_message{type = event,
                                      id = {server, 5},
                                      endpoint = <<"foo">>,
                                      data = <<"bar">>}) =:= <<"5:5:foo:bar">>)].

ecode_batch_test_() ->
    [?_assert(encode_batch([]) == <<"">>),
     ?_assert(encode_batch([#sio_message{}, #sio_message{}]) =:= <<?M,"4", ?M, "8:::", ?M, "4", ?M, "8:::">>),
     ?_assert(encode_batch([#sio_message{type = disconnect}, #sio_message{type = connect}, #sio_message{type = heartbeat}]) =:=
                  <<?M,"4", ?M, "0:::", ?M,"4", ?M, "1:::", ?M, "4", ?M, "2:::">>)].

split_test_() ->
    [?_assert(split(<<"">>) =:= []),
     ?_assert(split(<<"foobar">>) =:= [<<"foobar">>]),
     ?_assert(split(<<?M, "3", ?M, "foobar">>) =:= [<<"foo">>, <<"bar">>]),
     ?_assert(split(<<?M, "3", ?M, "foobar">>) =:= [<<"foo">>, <<"bar">>]),
     ?_assert(split(<<?M, "5", ?M, "a", ?M ,"b">>) =:= [<<"a", ?M, "b">>]),
     ?_assert(split(encode_batch([#sio_message{}, #sio_message{}])) =:= [encode(#sio_message{}), encode(#sio_message{})])].

decode_test_() ->
    lists:map(fun(T) ->
                      M = #sio_message{type = T},
                      ?_assert(decode(encode(M)) =:= M)
              end,
              ?MESSAGE_TYPES)
        ++
        [?_assert(decode(?NOOP) =:= #sio_message{type = noop}),
         ?_assert(decode(<<"8::">>) =:= #sio_message{type = noop}),
         ?_assert(decode(?ERROR("herped-derped")) =:= #sio_message{type = error, data = <<"herped-derped">>}),
         ?_assert(decode(<<"1:2:3:4">>) =:= #sio_message{type = connect, id = {server, 2}, endpoint = <<"3">>, data = <<"4">>}),
         ?_assert(decode(<<"1:2+:3:4">>) =:= #sio_message{type = connect, id = {client, 2}, endpoint = <<"3">>, data = <<"4">>})].

decode_batch_test_() ->
    lists:map(fun(M) ->
                      ?_assert(decode_batch(encode_batch(M)) =:= M)
              end,
              [[#sio_message{}],
               [#sio_message{}, #sio_message{}],
               [#sio_message{type = ack}, #sio_message{type = json}],
               [#sio_message{type = ack}, #sio_message{type = json}, #sio_message{type = noop}]]).

decode_maybe_batch_test_() ->
    lists:map(fun(T) ->
                      M = #sio_message{type = T},
                      ?_assert(decode_maybe_batch(encode(M)) =:= [M])
              end,
              ?MESSAGE_TYPES)
        ++
        [?_assert(decode_maybe_batch(?NOOP) =:= [#sio_message{type = noop}]),
         ?_assert(decode_maybe_batch(<<"8::">>) =:= [#sio_message{type = noop}]),
         ?_assert(decode_maybe_batch(?ERROR("herped-derped")) =:= [#sio_message{type = error, data = <<"herped-derped">>}]),
         ?_assert(decode_maybe_batch(<<"1:2:3:4">>) =:=
                      [#sio_message{type = connect, id = {server, 2}, endpoint = <<"3">>, data = <<"4">>}]),
         ?_assert(decode_maybe_batch(<<"1:2+:3:4">>) =:=
                      [#sio_message{type = connect, id = {client, 2}, endpoint = <<"3">>, data = <<"4">>}])]
        ++
        lists:map(fun(M) ->
                          ?_assert(decode_maybe_batch(encode_batch(M)) =:= M)
                  end,
                  [[#sio_message{}],
                   [#sio_message{}, #sio_message{}],
                   [#sio_message{type = ack}, #sio_message{type = json}],
                   [#sio_message{type = ack}, #sio_message{type = json}, #sio_message{type = noop}]]).
