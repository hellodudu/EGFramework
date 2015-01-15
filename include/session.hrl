-define(CONNECTED, connected).
-define(LOGGED_IN, logged_in).
-define(DISCONNECTED,disconnected).

-record( session, {
                   account_id,
                   socket,
                   transport,
                   connector_pid,
                   role_id,
                   riak_mem,
                   riak_db,
                   session_state = ?DISCONNECTED
                  } ).
