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


How to Compile
==================

### 1. about protocol
The Message format between server and client is as follows:<br>
message length + message num + message body<br>
the detailed description are as follows:
* message length: 2 bytes of big endian about the message length and parsed by erlang automatically with {packet,2} option, maybe later {packet,4} is more approriate.
* message num: 2 bytes of big endian number to indicate message name.Numbers are generated using proto_mapper.cc file under src/proto directory.  
* message body: Binaries with google protocol buff encoding. 

### 2. compile protocol mapper
compile src/proto/proto_mapper.cc into one executable file using proto buff libs, for example, under my mbp environment, use this following command:<br>
    clang++ -o proto_mapper -I/usr/local/include proto_mapper.cc /usr/local/lib/libprotobuf.a /usr/local/lib/libprotoc.a

### 3. compile *.erl
Code compilation and server initialization is controlled using ctl.py file.
First, execute <font color="red">./ctl.py build</font> to generate erlang tools; second, execute <font color="red">./ctl.py proto</font> to generate google protocol buff files; Finally <font color="red">./ctl.py build</font> to build all *.erl files to *.beams.

How to Start Server
-------------------------
Prepare riak database before start game server and 
execute <font color="red">./ctl.py start</font>to start the server.
for more on how to start riak, please visit:[riak](http://docs.basho.com).
Currently, a connector is started on node 'server1_connector1@127.0.0.1',
a game server is started on node 'server1_game1@127.0.0.1',
configurations are supported later.
