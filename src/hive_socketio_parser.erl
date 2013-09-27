-module(hive_socketio_parser).
-author('kajetan.rzepecki@zadane.pl').

-export([decode_maybe_batch/1, encode_batch/1, decode_batch/1, encode/1, decode/1]).

-ifdef(TEST).
-compile(export_all). %% NOTE For testing convenience.
-endif.

-include("hive_socketio.hrl").

-define(FRAME_MARKER, 16#fffd).

encode_batch([]) ->
    <<"">>;

encode_batch([Message | Rest]) ->
    Msg = encode(Message),
    Len = integer_to_binary(byte_size(Msg)),
    Frame = <<?FRAME_MARKER/utf8, Len/binary, ?FRAME_MARKER/utf8, Msg/binary>>,
    Rest2 = encode_batch(Rest),
    <<Frame/binary, Rest2/binary>>.

encode(Message) when is_binary(Message) ->
    %% NOTE Assumes valid format and whatnot. Use sparingly.
    Message;

encode(SIOMessage = #sio_message{}) ->
    Type = integer_to_binary(indexof(SIOMessage#sio_message.type, ?MESSAGE_TYPES) - 1),
    Endpoint = SIOMessage#sio_message.endpoint,
    Data = SIOMessage#sio_message.data,
    Id = case SIOMessage#sio_message.id of
             {client, I} -> Int = integer_to_binary(I),
                            <<Int/binary, "+">>;
             {server, I} -> integer_to_binary(I);
             undefined   -> <<"">>
         end,
    <<Type/binary, ":", Id/binary, ":", Endpoint/binary, ":", Data/binary>>.

indexof(Element, List) ->
    indexof(Element, List, 1). %% NOTE 1-based to keep Erlangs convention.

indexof(Element, [Element | _], Number) ->
    Number;

indexof(Element, [_ | Tail], Number) ->
    indexof(Element, Tail, Number+1).

decode_maybe_batch(Payload) ->
    case Payload of
        <<?FRAME_MARKER/utf8, _Rest/binary>> -> decode_batch(Payload);
        _                                    -> [decode(Payload)]
    end.

decode_batch(Payload) ->
    Messages = split(Payload),
    lists:map(fun decode/1, Messages).

split(Payload) ->
    Messages = split(Payload, []),
    lists:reverse(Messages).

split(<<?FRAME_MARKER/utf8, Rest/binary>>, Accumulator) ->
    [Len, Payload] = binary:split(Rest, <<?FRAME_MARKER/utf8>>),
    Length = list_to_integer(binary_to_list(Len)),

    Message = binary:part(Payload, 0, Length),
    Next = binary:part(Payload, Length, byte_size(Payload)-Length),

    split(Next, [Message | Accumulator]);

split(<<>>, Accumulator) ->
    Accumulator;

split(Message, Accumulator) ->
    [Message | Accumulator].

%% T:I:E:D
decode(Message) ->
    {Type, Rest} = get_type(Message),
    {Id, Rest2} = get_id(Rest),
    {Endpoint, Rest3} = get_endpoint(Rest2),
    Data = get_data(Rest3),
    #sio_message{type = Type, id = Id, endpoint = Endpoint, data = Data}.

%% T:I:E:D
get_type(Message) ->
    [TypePart, Rest] = binary:split(Message, <<":">>),
    Index = binary:first(TypePart) - $0, %% First byte - '0' ASCII value
    Type = lists:nth(Index+1, ?MESSAGE_TYPES),
    {Type, Rest}.

%% I:E:D
get_id(Message) ->
    [IdPart, Rest] = binary:split(Message, <<":">>),
    try binary:last(IdPart) of
        $+ -> Id = binary_to_integer(binary:part(IdPart, 0, byte_size(IdPart)-1)),
              {{client, Id}, Rest};
        _  -> {{server, binary_to_integer(IdPart)}, Rest}
    catch
        error:_ -> {undefined, Rest}
    end.

%% E:D
get_endpoint(Message) ->
    case binary:split(Message, <<":">>) of
        [Endpoint, Rest] -> {Endpoint, Rest};
        [Endpoint]       -> {Endpoint, <<>>}
    end.

%% D
get_data(Message) ->
    %% NOTE Data is optional, so whatever gets here is fine as-is.
    Message.
