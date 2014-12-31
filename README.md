Erlang Game Server
==================
an erlang game server based on ranch, google protocol buffer and riak.
for more about ranch, google protocol buffer, and riak, please refer to
[ranch](https://github.com/ninenines/ranch),
[google protocol buffer](https://developers.google.com/protocol-buffers),
[riak](http://docs.basho.com).<br>

This framework cludes:<br>
* a cluster of connector based on ranch for managing tcp front end session,
* a game application to handle various game logic.

Every player has a connector process, a game logic process( i.e. the role.erl gen_server ), and a riak client connection.
    

