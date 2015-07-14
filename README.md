Rebar管理多个app如何配置
==================
使用rebar来管理多个app，使其可以在主目录下编译、清除、打包所有子app，并且所有子app都可以共享一份依赖开源库。

目录结构如下：
simple-erlang-game/.......................
                  /apps/..................
                       /client/...........
                              /src/.......
                              rebar.config
                       /db/...............
                              /src/.......
                              rebar.config
                       /game/.............
                              /src/.......
                              rebar.config
                  /deps/..................
                  /ebin/..................
                  rebar.config
                  .
                  .
                  .

需要修改配置的地方：
1. 主目录下的rebar.config需要添加sub_dirs字段来标明子app和其rebar.config所在路径，比如这个项目在simple-erlang-game/rebar.config中添加一行：{sub_dirs, ["apps/db", "apps/game", "apps/client"]}.
2. 子目录下的rebar.config需要添加deps_dir字段来指定子app依赖的开源库所在目录，这里指定目录为"../../deps".

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
