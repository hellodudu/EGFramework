-module(emysql_test).

-export([add/0]).
-export([insert/0, select/0]).

add() ->
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(my_pool, [{size, 1}, {user, "root"}, {password, "123qwe"}, {database, "db_game"}, {encoding, utf8}]).

insert() ->
    emysql:execute(db_game, <<"INSERT INTO role SET name = 'dudu' age = 20">>).

select() ->
    Result = emysql:execute(my_pool, <<"SELECT name, age FROM role">>),
    io:format("result = ~p~n", [Result]).
