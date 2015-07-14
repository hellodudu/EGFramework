如何编译
==================
1. 安装emysql:
    mac下在终端下输入命令:brew install emysql，ubuntu下在终端下输入命令:apt-get install emysql。

2. 安装rebar:
    mac下终端输入命令:brew install rebar，linux下在 https://github.com/basho/rebar 下载源码编译或者直接下载rebar二进制文件，之后将rebar拷贝到/usr/local/bin/中，就能直接使用rebar命令了。

3. cd进入simple-erlang-game目录后，终端中执行./start.py rebuild，第一次编译时需要获取依赖库，编译时会提示缺少几个.erl文件，再运行一次./start.py rebuild就能自动生成缺少的文件了，之后再有代码修改可以直接运行./start.py build。


开启服务器
==================
1. 重置数据库:
    执行命令:./start.py reset_db

2. 开启game节点:
    执行命令:./start.py start_game

3. 开启db_session节点:
    新建一个终端并执行命令:./start.py start_db

4. 如果要后台运行服务器执行命令:./start.py start，可以通过命令:erl -setcookie server -name test@127.0.0.1 -remsh game@127.0.0.1来attach game节点
    运行命令:erl -setcookie server -name test@127.0.0.1 -remsh db_session@127.0.0.1来attach上db_session节点。

5. 终止服务器:
    执行命令:./start.py stop来结束game和db_session节点进程。
 
Rebar管理多个app如何配置
==================
使用rebar来管理多个app，使其可以在主目录下编译、清除、打包所有子app，并且所有子app都可以共享一份依赖开源库。

1. 目录结构如下：
simple-erlang-game/
    /apps/
        /client/
            /src/
            rebar.config
        /db/
            /src/
            /rel/
               reltool.config 
            rebar.config
        /game/
            /src/
            /rel/
               reltool.config
            rebar.config
    /deps/
    /ebin/
    rebar.config
                  .
                  .
                  .

2. 需要修改配置的地方：
    (1. 主目录下的rebar.config需要添加sub_dirs字段来标明子app和其rebar.config所在路径，比如这个项目在simple-erlang-game/rebar.config中添加一行：{sub_dirs, ["apps/db", "apps/game", "apps/client"]}.
    (2. 子目录下的rebar.config需要添加deps_dir字段来指定子app依赖的开源库所在目录，这里指定目录为"../../deps"，这样所有的app都会共享一份deps。
    (3. 子目录下的src/rel/reltool.config需要修改lib_dirs字段为{lib_dirs, ["../../../deps"]}，app那一行末尾添加一条属性{lib_dir, ".."}。

3. 
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
