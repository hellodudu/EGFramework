-module(db_session_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    crypto:start(),
    emysql:add_pool(my_pool, [{size, 1}, {user, "root", {password, "123qwe"}, {database, "db_game"}, {encoding, utf8}}]),
    db_session:start().

stop(State) ->
    lager:info( "dbsession stopped at node ~p", [erlang:node()]),
    State.


