-module(hive_websocket_handler).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(cowboy_websocket_handler).

-export([init/3, terminate/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-import(hive_http_utils, [reply/3, origin_valid/1]).

-include("hive_socketio.hrl").
-record(state, {sid = undefined, pid = undefined, ping_timer = undefined}).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1, incby/2]).

%% Cowboy websocket handler callbacks
init({tcp, http}, Request, _Options) ->
    inc(?HTTP_REQUESTS),
    case origin_valid(Request) of
        {ok, _Origin} ->
            {upgrade, protocol, cowboy_websocket};

        {error, Error} ->
            Req = reply(403, hive_error_utils:make_json(Error), Request),
            inc(?HTTP_ERRORS),
            {shutdown, Req, bad_origin}
    end.

terminate(_Reason, _Request, _State) ->
    ok.

websocket_init(_Transport, Request, _Options) ->
    inc(?WS_REQUESTS),
    {Sid, Request2} = cowboy_req:binding(sid, Request),
    case cowboy_req:qs_val(<<"disconnect">>, Request2) of
        {undefined, Request3} ->
            lager:info("Established a WebSocket connection to: ~s!", [Sid]),
            Sink = make_sink(),
            hive_router:set_sink(Sid, Sink),
            {ok, Request3, #state{sid = Sid}, hibernate};

        {_Value, Request3} ->
            lager:info("Disconnecting Client: ~p", [Sid]),
            hive_router:kill_client(Sid),
            {shutdown, Request3}
    end.

websocket_terminate({error, Reason}, _Request, State) ->
    log_terminate({error, Reason}, State);

websocket_terminate({remote, Reason}, _Request, State) ->
    log_terminate({remote, Reason}, State);

websocket_terminate(_Reason, _Request, _State) ->
    ok.

%% Cowboy websocket handler handlers
websocket_info(finalize_handshake, Request, State) ->
    lager:info("Finalizing a WebSocket handshake..."),
    Sid = State#state.sid,
    hive_router:upgrade_client(Sid, websocket),
    log_reply(ok),
    {reply, {text, ?CONNECT}, Request, State, hibernate};

websocket_info(terminate, Request, State) ->
    lager:info("Terminating a WebSocket connection."),
    log_reply(ok),
    {reply, {text, ?DISCONNECT}, Request, State, hibernate};

websocket_info({timeout, _Ref, ping_timeout}, Request, State) ->
    websocket_terminate({error, ping_timeout}, Request, State),
    {ok, Request, State, hibernate};

websocket_info({reply, []}, Request, State) ->
    log_reply(ok),
    Timeout = hive_config:get(<<"hive.websocket_ping_timeout">>, 500),
    Timer = erlang:start_timer(Timeout, self(), ping_timeout),
    {reply, [{text, ?NOOP}, {ping, <<"">>}], Request, State#state{ping_timer = Timer}};

websocket_info({reply, Messages}, Request, State) when is_list(Messages) ->
    Payload = lists:map(fun(Message) ->
                                {text, Message}
                        end,
                        Messages),
    log_reply(ok, length(Messages)),
    {reply, Payload, Request, State};

websocket_info({register, Pid}, Request, State) ->
    lager:info("Client ~p registered, skipping Hive router for this connection.", [Pid]),
    {ok, Request, State#state{pid = Pid}, hibernate};

websocket_info(Info, Request, State) ->
    inc(?WS_ERRORS),
    lager:warning("Unhandled Hive WebSocket Client message: ~p", [Info]),
    {ok, Request, State, hibernate}.

websocket_handle({pong, _Pong}, Request, State) ->
    %% NOTE Connection is well and kicking, so we don't need to terminate it.
    case State#state.ping_timer of
        undefined -> ok;
        Timer     -> erlang:cancel_timer(Timer)
    end,
    {ok, Request, State#state{ping_timer = undefined}, hibernate};

websocket_handle({text, Payload}, Request, State) ->
    Message = hive_socketio_parser:decode(Payload),
    log_msg(),
    case State#state.pid of
        undefined ->
            case hive_router:route_messages(State#state.sid, Message) of
                ok             -> {ok, Request, State, hibernate};
                {error, Error} -> inc(?WS_ERRORS),
                                  lager:warning("Hive WebSocket Client is unable to route messages: ~p", [Error]),
                                  ErrorMsg = hive_socketio_parser:encode(hive_error_utils:make_reply(Error)),
                                  log_reply(bad),
                                  {reply, [{text, ErrorMsg}, close], Request, State}
            end;
        Pid ->
            %% NOTE This skips the router alltogether rendering any special logic,
            %% NOTE such as logging, impossible.
            %% NOTE This can be turned off in the configuration file.
            hive_client:process_messages(Pid, Message),
            {ok, Request, State, hibernate}
    end;

websocket_handle(Frame, Request, State) ->
    inc(?WS_ERRORS),
    lager:warning("Unhandled Hive WebSocket Client frame type: ~p", [Frame]),
    log_msg(),
    log_reply(bad),
    {reply, {text, ?ERROR("Unknown WebSocket frame type!")}, Request, State, hibernate}.

%% Internal functions:
make_sink() ->
    self().

log_reply(Type) ->
    log_reply(Type, 1).

log_reply(ok, Num) ->
    incby(?WS_OK, Num);

log_reply(bad, Num) ->
    incby(?WS_BAD, Num).

log_msg() ->
    log_msg(1).

log_msg(Num) ->
    incby(?WS_MSGS, Num).

log_terminate(Reason, State) ->
    inc(?WS_HANG),
    lager:warning("WebSocket connection terminated abruptly: ~p", [Reason]),
    case State#state.pid of
        undefined -> hive_router:kill_client(State#state.sid);
        Pid       -> hive_client:terminate(Pid, shutdown)
    end,
    ok.
