-author('kajetan.rzepecki@zadane.pl').

%% Monitor related defines, mostly counters.
%% NOTE Each of these has to have a distinct name.

%% Hive related counters:
-define(HIVE_UPTIME,             <<"hive.uptime">>).
-define(HIVE_ERRORS,             <<"hive.errors">>).
-define(TOTAL_PROCESSES,         <<"hive.total_processes">>).
-define(HIVE_PLUGINS,            <<"hive.plugins">>).
-define(HIVE_PLUGIN_ERRORS,      <<"hive.plugin_errors">>).
-define(CONFIG_ERRORS,           <<"hive.config_errors">>).

%% Hive cluster management related counters:
-define(HIVE_CLUSTER_ERRORS,     <<"hive.cluster.errors">>).

%% Hive memory usage related:
-define(TOTAL_MEMORY,            <<"hive.memory.total">>).
-define(PROCESSES_MEMORY,        <<"hive.memory.processes">>).
-define(SYSTEM_MEMORY,           <<"hive.memory.system">>).
-define(ATOM_MEMORY,             <<"hive.memory.atom">>).
-define(BINARY_MEMORY,           <<"hive.memory.binary">>).
-define(CODE_MEMORY,             <<"hive.memory.code">>).
-define(ETS_MEMORY,              <<"hive.memory.ets">>).

%% Router related counters:
-define(ROUTER_UPTIME,           <<"hive.router.uptime">>).
-define(TOTAL_SPAWNS,            <<"hive.router.spawned_clients">>).
-define(ROUTER_CLIENT_NUM,       <<"hive.router.current_clients">>).
-define(ROUTER_REQUESTS,         <<"hive.router.requests">>).
-define(ROUTER_QUEUE,            <<"hive.router.msg_queue_length">>).
-define(ROUTED_MSGS,             <<"hive.router.routed_msgs">>).
-define(ROUTED_EVENTS,           <<"hive.router.routed_events">>).
-define(ROUTER_ERRORS,           <<"hive.router.errors">>).

%% Client related counters:
-define(CLIENT_NUM,              <<"clients.total">>).
-define(WS_CLIENT_NUM,           <<"clients.websocket">>).
-define(XHR_CLIENT_NUM,          <<"clients.xhr_polling">>).
-define(CLIENT_ERRORS,           <<"clients.errors">>).

%% Client FSM related counters:
-define(GENERIC_NUM,             <<"clients.states.generic">>).
-define(TRANSIENT_NUM,           <<"clients.states.transient">>).
-define(WAITING_NUM,             <<"clients.states.waiting">>).
-define(POLLING_NUM,             <<"clients.states.polling">>).
-define(TRANSITIONS_NUM,         <<"clients.states.transitions">>).

%% Client state related counters:
-define(STATE_MGR_REQUESTS,      <<"clients.state_mgr.requests">>).
-define(STATE_MGR_ERRORS,        <<"clients.state_mgr.errors">>).
-define(STATE_MGR_GETS,          <<"clients.state_mgr.get">>).
-define(STATE_MGR_SETS,          <<"clients.state_mgr.set">>).
-define(STATE_MGR_INITS,         <<"clients.state_mgr.init">>).
-define(STATE_MGR_CLEANUPS,      <<"clients.state_mgr.cleanup">>).

%% Event (Socket.IO message) related counters:
-define(TOTAL_EVENTS,            <<"clients.events.total">>).
-define(TOTAL_EVENT_ERRORS,      <<"clients.events.errors">>).
-define(EXTERNAL_EVENTS,         <<"clients.events.external">>).
-define(INTERNAL_EVENTS,         <<"clients.events.internal">>).
-define(HIVE_EVENTS,             <<"clients.events.control">>).
-define(INTERNAL_EVENT_ERRORS,   <<"clients.events.internal_errors">>).
-define(EXTERNAL_EVENT_ERRORS,   <<"clients.events.external_errors">>).
-define(HIVE_EVENT_ERRORS,       <<"clients.events.control_errors">>).

%% HTTP/WS related counters:
-define(HTTP_REQUESTS,           <<"clients.transports.http.requests">>).
-define(HTTP_ERRORS,             <<"clients.transports.http.errors">>).
-define(HTTP_2XX,                <<"clients.transports.http.2XX">>).
-define(HTTP_4XX,                <<"clients.transports.http.4XX">>).
-define(HTTP_5XX,                <<"clients.transports.http.5XX">>).
-define(HTTP_UNKNOWN,            <<"clients.transports.http.???">>).
-define(HTTP_HANG,               <<"clients.transports.http.hang_up">>).
-define(WS_REQUESTS,             <<"clients.transports.websocket.requests">>).
-define(WS_ERRORS,               <<"clients.transports.websocket.errors">>).
-define(WS_MSGS,                 <<"clients.transports.websocket.frames">>).
-define(WS_OK,                   <<"clients.transports.websocket.ok">>).
-define(WS_BAD,                  <<"clients.transports.websocket.bad">>).
-define(WS_HANG,                 <<"clients.transports.websocket.hang_up">>).

%% Client Hooks related counters:
-define(HOOK_CALLS,              <<"clients.hooks.calls">>).
-define(HOOK_ERRORS,             <<"clients.hooks.errors">>).

-define(HOOK_HP_PUT,             <<"clients.hooks.hp.put">>).
-define(HOOK_HP_POST,            <<"clients.hooks.hp.post">>).
-define(HOOK_HP_GET,             <<"clients.hooks.hp.get">>).

-define(HOOK_PUBSUB_PUB,         <<"clients.hooks.pubsub.publish">>).
-define(HOOK_PUBSUB_SUB,         <<"clients.hooks.pubsub.subscribe">>).
-define(HOOK_PUBSUB_UNSUB,       <<"clients.hooks.pubsub.unsubscribe">>).

-define(HOOK_EVENT_CALLS,        <<"clients.hooks.$(name).calls">>).
-define(HOOK_EVENT_ERRORS,       <<"clients.hooks.$(name).errors">>).

-define(HOOK_EVENT_HP_PUT,       <<"clients.hooks.$(name).hp.put">>).
-define(HOOK_EVENT_HP_POST,      <<"clients.hooks.$(name).hp.post">>).
-define(HOOK_EVENT_HP_GET,       <<"clients.hooks.$(name).hp.get">>).

-define(HOOK_EVENT_PUBSUB_PUB,   <<"clients.hooks.$(name).pubsub.publish">>).
-define(HOOK_EVENT_PUBSUB_SUB,   <<"clients.hooks.$(name).pubsub.subscribe">>).
-define(HOOK_EVENT_PUBSUB_UNSUB, <<"clients.hooks.$(name).pubsub.unsubscribe">>).

%% Hive Connectors counters:
-define(CONNECTORS_REQUESTS,     <<"connectors.requests">>).
-define(CONNECTORS_ERRORS,       <<"connectors.errors">>).
-define(CONNECTORS_POOLS,        <<"connectors.pools">>).
-define(CONNECTORS_SPAWN,        <<"connectors.starts">>).
-define(CONNECTORS_STOP,         <<"connectors.stops">>).
-define(CONNECTORS_USE,          <<"connectors.uses">>).
-define(CONNECTORS_DO_SAFE,      <<"connectors.safe_transactions">>).
-define(CONNECTORS_DO_UNSAFE,    <<"connectors.unsafe_transactions">>).
-define(CONNECTORS_RENT,         <<"connectors.rents">>).
-define(CONNECTORS_RETURN,       <<"connectors.returns">>).

-define(CONN_HTTP_CONNECTORS,    <<"connectors.http.$(name).workers">>).
-define(CONN_HTTP_REQUESTS,      <<"connectors.http.$(name).requests">>).
-define(CONN_HTTP_ERRORS,        <<"connectors.http.$(name).errors">>).
-define(CONN_HTTP_GET,           <<"connectors.http.$(name).sync_gets">>).
-define(CONN_HTTP_POST,          <<"connectors.http.$(name).sync_posts">>).
-define(CONN_HTTP_APOST,         <<"connectors.http.$(name).async_posts">>).

-define(CONN_REDIS_CONNECTORS,   <<"connectors.redis.$(name).workers">>).
-define(CONN_REDIS_REQUESTS,     <<"connectors.redis.$(name).requests">>).
-define(CONN_REDIS_ERRORS,       <<"connectors.redis.$(name).errors">>).
-define(CONN_REDIS_QUERIES ,     <<"connectors.redis.$(name).queries">>).

-define(CONN_TCP_CONNECTORS,     <<"connectors.tcp.$(name).workers">>).
-define(CONN_TCP_REQUESTS,       <<"connectors.tcp.$(name).requests">>).
-define(CONN_TCP_ERRORS,         <<"connectors.tcp.$(name).errors">>).
-define(CONN_TCP_SEND,           <<"connectors.tcp.$(name).send">>).
-define(CONN_TCP_RECV,           <<"connectors.tcp.$(name).recv">>).

%% Hive Pub-Sub counters:
-define(PUBSUB_REQUESTS,         <<"pubsub.requests">>).
-define(PUBSUB_ERRORS,           <<"pubsub.errors">>).
-define(PUBSUB_CHANNELS,         <<"pubsub.total_channels">>).
-define(PUBSUB_STATUS,           <<"pubsub.status">>).
-define(PUBSUB_SUBSCRIBE,        <<"pubsub.subscribe">>).
-define(PUBSUB_UNSUBSCRIBE,      <<"pubsub.unsubscribe">>).
-define(PUBSUB_JOIN,             <<"pubsub.join">>).
-define(PUBSUB_LEAVE,            <<"pubsub.leave">>).
-define(PUBSUB_PUBLISH,          <<"pubsub.publish">>).
-define(PUBSUB_PUBLISHED,        <<"pubsub.published_events">>).

-define(PUBSUB_CHANNEL_REQUESTS, <<"pubsub.channels.$(name).requests">>).
-define(PUBSUB_CHANNEL_ERRORS,   <<"pubsub.channels.$(name).errors">>).
-define(PUBSUB_CHANNEL_CHANNELS, <<"pubsub.channels.$(name).total_channels">>).
-define(PUBSUB_CHANNEL_STATUS,   <<"pubsub.channels.$(name).status">>).
-define(PUBSUB_CHANNEL_SUBS,     <<"pubsub.channels.$(name).subscribe">>).
-define(PUBSUB_CHANNEL_UNSUBS,   <<"pubsub.channels.$(name).unsubscribe">>).
-define(PUBSUB_CHANNEL_PUBLISH,  <<"pubsub.channels.$(name).publish">>).
-define(PUBSUB_CHANNEL_SUBSCR,   <<"pubsub.channels.$(name).subscribed_clients">>).
-define(PUBSUB_CHANNEL_PUBLISHED,<<"pubsub.channels.$(name).published_events">>).

%% Hive API counters:
-define(API_REQUESTS,            <<"api.requests">>).
-define(API_ERRORS,              <<"api.errors">>).
-define(API_HIVE_REQUESTS,       <<"api.hive.requests">>).
-define(API_HIVE_ERRORS,         <<"api.hive.errors">>).
-define(API_ROUTER_REQUESTS,     <<"api.router.requests">>).
-define(API_ROUTER_ERRORS,       <<"api.router.errors">>).
-define(API_PUBSUB_REQUESTS,     <<"api.pubsub.requests">>).
-define(API_PUBSUB_ERRORS,       <<"api.pubsub.errors">>).
-define(API_CLIENTS_REQUESTS,    <<"api.clients.requests">>).
-define(API_CLIENTS_ERRORS,      <<"api.clients.errors">>).

%% For pre-initialization:
%% NOTE Since Connector Pool counters have dynamically generated names, we have to initialize them separately.
-define(COUNTERS, [?HIVE_UPTIME, ?HIVE_ERRORS, ?TOTAL_PROCESSES, ?TOTAL_MEMORY, ?PROCESSES_MEMORY, ?SYSTEM_MEMORY, ?ATOM_MEMORY,
                   ?BINARY_MEMORY, ?CODE_MEMORY, ?ETS_MEMORY, ?ROUTER_UPTIME, ?TOTAL_SPAWNS, ?ROUTER_CLIENT_NUM, ?ROUTER_REQUESTS,
                   ?ROUTER_QUEUE, ?ROUTED_MSGS, ?ROUTED_EVENTS, ?ROUTER_ERRORS, ?CLIENT_NUM, ?WS_CLIENT_NUM, ?XHR_CLIENT_NUM,
                   ?CLIENT_ERRORS, ?GENERIC_NUM, ?TRANSIENT_NUM, ?WAITING_NUM, ?POLLING_NUM, ?TRANSITIONS_NUM, ?STATE_MGR_GETS,
                   ?STATE_MGR_SETS, ?STATE_MGR_INITS, ?STATE_MGR_CLEANUPS, ?STATE_MGR_REQUESTS, ?STATE_MGR_ERRORS, ?TOTAL_EVENTS,
                   ?EXTERNAL_EVENTS, ?INTERNAL_EVENTS, ?HIVE_EVENTS, ?INTERNAL_EVENT_ERRORS, ?EXTERNAL_EVENT_ERRORS, ?HIVE_EVENT_ERRORS,
                   ?HTTP_REQUESTS, ?HTTP_ERRORS, ?HTTP_2XX, ?HTTP_4XX, ?HTTP_5XX, ?HTTP_UNKNOWN, ?HTTP_HANG, ?WS_REQUESTS, ?WS_ERRORS,
                   ?WS_MSGS, ?WS_OK, ?WS_BAD, ?WS_HANG, ?HOOK_CALLS, ?HOOK_ERRORS, ?HOOK_PUBSUB_SUB, ?HOOK_PUBSUB_UNSUB, ?API_PUBSUB_REQUESTS,
                   ?CONNECTORS_REQUESTS, ?CONNECTORS_ERRORS, ?CONNECTORS_POOLS, ?CONNECTORS_SPAWN, ?CONNECTORS_STOP, ?CONNECTORS_DO_SAFE,
                   ?CONNECTORS_RENT, ?HOOK_PUBSUB_PUB, ?API_CLIENTS_ERRORS, ?HOOK_HP_POST, ?HOOK_HP_PUT, ?HOOK_HP_GET, ?TOTAL_EVENT_ERRORS,
                   ?CONNECTORS_RETURN, ?PUBSUB_REQUESTS, ?PUBSUB_ERRORS, ?PUBSUB_CHANNELS, ?PUBSUB_STATUS, ?PUBSUB_SUBSCRIBE,
                   ?PUBSUB_UNSUBSCRIBE, ?PUBSUB_JOIN, ?PUBSUB_LEAVE, ?PUBSUB_PUBLISH, ?PUBSUB_PUBLISHED, ?API_REQUESTS, ?API_ERRORS,
                   ?API_PUBSUB_ERRORS, ?API_CLIENTS_REQUESTS, ?API_HIVE_REQUESTS, ?API_HIVE_ERRORS, ?API_ROUTER_REQUESTS,
                   ?API_ROUTER_ERRORS, ?HIVE_CLUSTER_ERRORS]).
