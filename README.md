Erlang Game Server
==================
an erlang game server based on ranch, google protocol buffer and riak.
It cludes:
    a cluster of connector based on ranch for managing tcp front end session,
    a game application to handle various game logic.
Every player has a connector process, a game logic process( i.e. the role.erl gen_server ), and a riak client connection.
    

