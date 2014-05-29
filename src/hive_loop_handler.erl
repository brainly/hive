-module(hive_loop_handler).
-author('kajetan.rzepecki@zadane.pl').
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).
-import(hive_http_utils, [reply/2, reply/3, origin_valid/1]).

-include("hive_monitor.hrl").
-import(hive_monitor_utils, [inc/1]).

%% Cowboy loop handler callbacks
init({tcp, http}, Request, _Options = [HBTimeout, ReconnectTimeout, Transports]) ->
    inc(?HTTP_REQUESTS),

    case origin_valid(Request) of
        {ok, _Origin} ->
            case cowboy_req:method(Request) of
                {<<"GET">>, Req} ->
                    lager:info("Initializing a new Socket.IO connection!"),
                    %% NOTE The real heartbeat_timeout should be lower than the one sent to the Browser Client.
                    SocketIOOptions = [round(1.1 * HBTimeout), ReconnectTimeout, concat(Transports, <<",">>)],
                    %% NOTE The underlying handler module is swappable, but swapping them makes
                    %% NOTE little sense since Hive Hooks were implemented.
                    %% NOTE If you insist on adding swappable handlers to the configuration file,
                    %% NOTE just load the value here and pass it to hive_router:new_client().
                    case hive_router:new_client(hive_hooks_client) of
                        {ok, Sid}      ->
                            {ok, Req, {initialize_handshake, handshake(Sid, SocketIOOptions)}};

                        {error, {router_error, client_slots_limit}} ->
                            Req2 = reply(403, <<"">>, Req),
                            {shutdown, Req2, internal_error};

                        {error, {Code, Error}} ->
                            inc(?HTTP_ERRORS),
                            lager:debug("Hive Server encountered an error: ~p", [{Code, Error}]),
                            Req2 = reply(500, hive_error_utils:make_json(Code, Error), Req),
                            {shutdown, Req2, internal_error}
                    end;

                {Method, Req} ->
                    inc(?HTTP_ERRORS),
                    ErrorMsg = hive_error_utils:format("Unsupported method: ~s", [Method]),
                    lager:warning(ErrorMsg),
                    Req2 = reply(405, hive_error_utils:make_json(ErrorMsg), Req),
                    {shutdown, Req2, bad_method}
            end;

        {error, {bad_origin, Error}} ->
            inc(?HTTP_ERRORS),
            lager:debug("Hive Server encountered an error: ~p", [Error]),
            Req = reply(403, hive_error_utils:make_json(Error), Request),
            {shutdown, Req, bad_origin};

        {error, Error} ->
            inc(?HTTP_ERRORS),
            lager:debug("Hive Server encountered an error: ~p", [Error]),
            Req = reply(400, <<"">>, Request),
            {shutdown, Req, bad_origin}

    end.

terminate(_Reason, _Request, _State) ->
    ok.

%% Cowboy loop handler handlers
handle(Request, {initialize_handshake, Reply}) ->
    lager:info("Sending Socket.IO handshake: ~s", [Reply]),
    Req2 = reply(Reply, Request),
    {ok, Req2, ok}.

%% Internal functions:
handshake(Sid, Options) ->
    list_to_binary(io_lib:format("~s:~p:~p:~s", [Sid | Options])).

concat([], _Separator) ->
    <<"">>;

concat([Last], _Separator) ->
    Last;

concat([Bin | Rest], Separator) ->
    Catd = concat(Rest, Separator),
    <<Bin/binary, Separator/binary, Catd/binary>>.
