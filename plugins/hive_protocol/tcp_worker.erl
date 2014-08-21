-module(tcp_worker).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([start_link/4, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([kill/2]).

-include("hive_socketio.hrl").
-include("hive_connectors.hrl").
-import(hive_connectors_utils, [restart_timer/1, cancel_timer/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/2, dec/2]).
-define(inc(Counter), inc(Counter, State#state.pool_name)).
-define(dec(Counter), dec(Counter, State#state.pool_name)).

-record(data, {owner, socket, transport}).

%% Gen Server callbacks:
start_link(Ref, Socket, Transport, [Owner, Args]) ->
    gen_server:start_link(?MODULE, {Ref, Socket, Transport, Owner, Args}, []).

init({Ref, Socket, Transport, Owner, Args}) ->
    link(Owner), %% NOTE So the owner knows we're here.
    Name = proplists:get_value(pool_name, Args),
    RestartTimeout = proplists:get_value(<<"restart_timeout">>, Args),
    Timeout = proplists:get_value(<<"max_connection_timeout">>, Args, 5000),
    gen_server:cast(self(), {ack, Ref}),
    State = #state{
               pool_name = Name,
               lock = unlocked,
               data = #data{
                         owner = Owner,
                         transport = Transport,
                         socket = Socket
                        },
               restart_timer = undefined,
               restart_timeout = RestartTimeout,
               max_reconnect_timeout = Timeout
              },
    ?inc(?CONN_TCP_CONNECTORS),
    {ok, restart_timer(State)}.

terminate(_Reason, State) ->
    ?dec(?CONN_TCP_CONNECTORS),
    Data = State#state.data,
    Transport = Data#data.transport,
    Transport:close(Data#data.socket).

%% External functions:
kill(Client, Reason) ->
    gen_server:call(Client, {kill, Reason}).

%% NOTE The rest of the API is conformant to the Hive Protocol.

%% Gen Server handlers:
handle_call({kill, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call({get, Endpoint}, _From, State) ->
    ?inc(?CONN_TCP_REQUESTS),
    ?inc(?CONN_TCP_RECV),
    {reply, recv(Endpoint, State), State};

handle_call({put, Endpoint, Data}, _From, State) ->
    ?inc(?CONN_TCP_REQUESTS),
    ?inc(?CONN_TCP_RECV),
    {reply, send(Endpoint, Data, State), State};

handle_call({post, Endpoint, Data}, _From, State) ->
    ?inc(?CONN_TCP_REQUESTS),
    ?inc(?CONN_TCP_SEND),
    case send(Endpoint, Data, State) of
        ok             -> ?inc(?CONN_TCP_RECV),
                          {reply, recv(Endpoint, State), State};
        {error, Error} -> {reply, {error, Error}, State}
    end;

handle_call(Msg, _From, State) ->
    ?inc(?CONN_TCP_ERRORS),
    lager:warning("Unhandled Hive TCP Connector call: ~p", [Msg]),
    {reply, ok, State}.

handle_cast({ack, Ref}, State) ->
    ranch:accept_ack(Ref),
    Data = State#state.data,
    Timeout = State#state.max_reconnect_timeout,
    %% NOTE So we can find out whether a connection is dead or not.
    inet:setopts(Data#data.socket, [{send_timeout, Timeout}]),
    add_self(State),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, shutdown, State};

handle_cast(Msg, State) ->
    ?inc(?CONN_TCP_ERRORS),
    lager:warning("Unhandled Hive TCP Connector cast: ~p", [Msg]),
    {noreply, State}.

handle_info({timeout, _Ref, stop}, State) ->
    remove_self(State),
    {noreply, cancel_timer(State)};

handle_info(Info, State) ->
    ?inc(?CONN_TCP_ERRORS),
    lager:warning("Unhandled Hive TCP Connector info: ~p.", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    ?inc(?CONN_TCP_ERRORS),
    lager:warning("Unhandled Hive TCP Connector code change."),
    {ok, State}.

%% Internal functions:
recv(Endpoint, State) ->
    Data = State#state.data,
    Socket = Data#data.socket,
    Timeout = State#state.max_reconnect_timeout,
    Transport = Data#data.transport,
    case do_recv(Transport, Socket, Timeout) of
        {ok, Packet} ->
            [Message] = hive_socketio_parser:decode_batch(Packet),
            case Message of
                #sio_message{endpoint = Endpoint} ->
                    {ok, Message#sio_message.data};

                Otherwise ->
                    ?inc(?CONN_TCP_ERRORS),
                    ErrorMsg = hive_error_utils:format("Hive TCP Connector's received an unexpected message: ~p",
                                                       [Otherwise]),
                    lager:error(ErrorMsg),
                    {error, {tcp_error, ErrorMsg}}

            end;

        {error, Error} ->
            ?inc(?CONN_TCP_ERRORS),
            ErrorMsg = hive_error_utils:format("Hive TCP Connector's recv failed: ~p", [Error]),
            lager:error(ErrorMsg),
            maby_die(Error, State),
            {error, {tcp_error, ErrorMsg}}
    end.

do_recv(Transport, Socket, Timeout) ->
    case Transport:recv(Socket, 0, Timeout) of
        {ok, Packet} ->
            %% NOTE If by any chance a client receives malformed message here it should plainly die.
            {Len, Data} = hive_socketio_parser:msg_length(Packet),
            case byte_size(Data) of
                Len ->
                    {ok, Packet};

                L when L < Len ->
                    case Transport:recv(Socket, Len - L, Timeout)of
                        {ok, Rest}     -> {ok, <<Packet/binary, Rest/binary>>};
                        {error, Error} -> {error, Error}
                    end;

                _Otherwise ->
                    {error, <<"Wrong Socket.IO message length encoding.">>}
            end;

        {error, Error} ->
            {error, Error}
    end.

send(Endpoint, Message, State) ->
    Data = State#state.data,
    Socket = Data#data.socket,
    Transport = Data#data.transport,
    Packet = hive_socketio_parser:encode_batch([#sio_message{
                                                   type = message,
                                                   endpoint = Endpoint,
                                                   data = Message
                                                  }]),
    %% NOTE This operation might timeout because we've set send_timeout on initialization.
    case Transport:send(Socket, Packet) of
        {error, Error} -> ?inc(?CONN_TCP_ERRORS),
                          ErrorMsg = hive_error_utils:format("Hive TCP Connector's send failed: ~p", [Error]),
                          lager:error(ErrorMsg),
                          maby_die(Error, State),
                          {error, {tcp_error, ErrorMsg}};
        ok             -> ok
    end.

maby_die(closed, State) ->
    remove_self(State);

maby_die(_Otherwise, _State) ->
    no_thanks.

add_self(State) ->
    Data = State#state.data,
    tcp_connector:add(Data#data.owner, self()).

remove_self(State) ->
    Data = State#state.data,
    tcp_connector:remove(Data#data.owner, self()).
