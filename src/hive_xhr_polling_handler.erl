-module(hive_xhr_polling_handler).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(cowboy_http_handler).

-export([init/3, info/3, handle/2, terminate/3]).
-import(hive_http_utils, [reply/2, reply/3, origin_valid/1]).

-include("hive_socketio.hrl").
-record(state, {sid = undefined}).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

%% Cowboy http handler callbacks
init({tcp, http}, Request, _Options) ->
    inc(?HTTP_REQUESTS),
    {Sid, Request2} = cowboy_req:binding(sid, Request),
    case origin_valid(Request2) of
        {ok, _Origin} ->
            case cowboy_req:qs_val(<<"disconnect">>, Request2) of
                {undefined, Request3} ->
                    case cowboy_req:method(Request3) of
                        {<<"GET">>, Req} ->
                            Sink = make_sink(),
                            hive_router:set_sink(Sid, Sink),
                            %% NOTE Since there's no way to check whether set_sink failed,
                            %% NOTE each request has to have a safe timeout set for it.
                            Timeout = round(1.1 * (hive_config:get(<<"socketio.session_timeout">>) +
                                                       hive_config:get(<<"socketio.heartbeat_timeout">>))),
                            {loop, Req, #state{sid = Sid}, Timeout, hibernate};

                        {<<"POST">>, Req} ->
                            {ok, Req, {dispatch, Sid}};

                        {Method, Req} ->
                            ErrorMsg = hive_error_utils:format("Unsupported HTTP request method: ~s", [Method]),
                            lager:error(ErrorMsg),
                            Req2 = reply(405, hive_error_utils:make_json(ErrorMsg), Req),
                            {shutdown, Req2, error}
                    end;

                %% Client requested a disconnection:
                {_Value, Req} ->
                    {ok, Req, {disconnect, Sid}}
            end;

        {error, Error} ->
            Req = reply(403, hive_error_utils:make_json(Error), Request2),
            {shutdown, Req, error}
    end.

terminate(_Reason, _Request, done) ->
    ok;

terminate(_Reason, _Request, error) ->
    inc(?HTTP_ERRORS),
    ok;

terminate(Reason, _Request, State) ->
    inc(?HTTP_HANG),
    lager:warning("XHR-polling connection terminated abruptly: ~p", [Reason]),
    hive_router:kill_client(State#state.sid),
    ok.

%% Cowboy http handler handlers
handle(Request, {dispatch, Sid}) ->
    {ok, Payload, Req} = cowboy_req:body(Request),
    Messages = hive_socketio_parser:decode_maybe_batch(Payload),
    case hive_router:route_messages(Sid, Messages) of
        ok             -> Req2 = reply(<<"">>, Req),
                          {ok, Req2, done};
        {error, Error} -> lager:warning("Hive XHR-polling Client is unable to route messages: ~p", [Error]),
                          ErrorMsg = hive_socketio_parser:encode(hive_error_utils:make_reply(Error)),
                          inc(?HTTP_ERRORS),
                          Req2 = reply(500, ErrorMsg, Req),
                          {ok, Req2, error}
    end;

handle(Request, {disconnect, Sid}) ->
    lager:info("Disconnecting Client: ~s", [Sid]),
    hive_router:kill_client(Sid),
    Req = reply(?DISCONNECT, Request),
    {ok, Req, done};

handle(Request, Action) ->
    lager:warning("Unhandled Hive XHR-polling Client request: ~p", [Action]),
    {ok, Request, error}.

info(finalize_handshake, Request, State) ->
    Sid = State#state.sid,
    lager:info("Finalizing an XHR-polling handshake..."),
    hive_router:upgrade_client(Sid, xhr_polling),
    Req = reply(?CONNECT, Request),
    {ok, Req, done};

info({reply, <<"">>}, Request, _State) ->
    Req = reply(?NOOP, Request),
    {ok, Req, done};

info({reply, Messages}, Request, _State) when is_binary(Messages) ->
    Req = reply(Messages, Request),
    {ok, Req, done};

info(terminate, Request, _State) ->
    lager:info("Terminating an XHR-polling connection."),
    Req = reply(?DISCONNECT, Request),
    {ok, Req, done};

info(Info, Request, _State) ->
    lager:warning("Unhandled Hive XHR-polling Client info message: ~p", [Info]),
    {ok, Request, error}.

%% Internal functions
make_sink() ->
    self().
