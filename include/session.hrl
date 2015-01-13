-define(CONNECTED, connected).
-define(LOGGED_IN, logged_in).
-define(DISCONNECTED,disconnected).

-record( session, {
                   account_id,
                   socket,
                   transport,
                   connector_pid,
                   role_id,
                   role_pid,
                   riak_connection_pid,
                   session_state = ?DISCONNECTED
                  } ).
