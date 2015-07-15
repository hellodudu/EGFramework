如何编译
==================
##安装emysql
mac下在终端下输入命令

    $ brew install emysql

ubuntu下在终端下输入命令

    $ apt-get install emysql

##安装rebar
mac下终端输入命令

    $ brew install rebar

linux下在[这里](https://github.com/basho/rebar) 下载源码编译或者直接下载rebar二进制文件，之后将rebar拷贝到`/usr/local/bin/`中，就能直接使用rebar命令了

##开始编译
cd进simple-erlang-game目录，终端中执行
    
    $ ./start.py rebuild

第一次编译时需要获取依赖库，编译时会提示缺少几个.erl文件，再运行一次
    
    $ ./start.py rebuild

就能自动生成缺少的文件了，之后再有代码修改可以直接运行
    
    $ ./start.py build

这样不用执行`rebar get-deps`和生成`proto`的操作，会比
    
    $ ./start.py rebuild

更快


开启服务器
==================

##带终端的服务器

###重置数据库

    $ ./start.py reset_db

    将`src/tools/db_game.sql`注入到mysql中

###开启game节点

    $ ./start.py start_game

###开启db_session节点:
    新建一个终端并执行命令

    $ ./start.py start_db

    game和db_session节点都启动后会自动连接并且打印log到终端

##不带终端的服务器

###开启服务器

    $ ./start.py start 

    game和db_session节点都开启在后台

    可以通过命令
    
        $ erl -setcookie server -name test@127.0.0.1 -remsh game@127.0.0.1 
        来连接上`game`节点，`db_session`节点同理

    通过命令
        $ ps aux | grep application:ensure_all_started 
        来查看节点进程信息 

###终止服务器:

        $ ./start.py stop 
    结束`game`和`db_session`节点进程
 

Rebar管理多个app如何配置
==================
使用rebar来管理多个app，使其可以在主目录下编译、清除、打包所有子app，并且所有子app都可以共享一份依赖开源库。

## 目录结构


![mark1](/png/mark1.png)

## 需要修改配置的地方：
* 主目录下的`rebar.config`需要添加sub_dirs字段来标明子app和其rebar.config所在路径，比如这个项目在simple-erlang-game/rebar.config中添加一行`{sub_dirs, ["apps/db", "apps/game", "apps/client"]}.`


![mark2](/png/mark2.png)

* 子目录下的`rebar.config`需要添加一行`{deps_dir, "../../deps"}`字段来指定子app依赖的开源库所在目录，这里指定目录为`"../../deps"`，这样所有的app都会共享一份deps


![mark3](/png/mark3.png)

* 子目录下的`rebar.config`添加一行`{sub_dirs, ["rel"]}.`，指定打包release路径


![mark4](/png/mark4.png)

* 子目录下的`rel/reltool.config`需要修改`lib_dirs`字段为`{lib_dirs, ["../../../deps"]}`，app那一行末尾添加一条属性`{lib_dir, ".."}`


![mark5](/png/mark5.png)

## 发布

在主目录`simple-erlang-game`下输入命令

    $ rebar compile generate

或者

    $ ./start.py generate

执行命令后会在各个`app`子目录`rel/`中生成对应节点名称的文件夹，里面包含了所有节点运行时需要的库，可以直接运行在没有安装erlang环境的主机上


运行release版服务
======================

### 通过python运行

    开启

    $ ./start.py start_release

    链接

    $ ./start.py attach_rel_game
    $ ./start.py attach_rel_db

    关闭

    $ ./start.py stop_release

### 通过终端命令运行

    开启

    $ /apps/db_session/rel/db_session/bin/db_session start
    $ /apps/game/rel/game/bin/game start

    链接

    $ /apps/db_session/rel/db_session/bin/db_session attach
    $ /apps/game/rel/game/bin/game attach

    终止

    $ /apps/db_session/rel/db_session/bin/db_session stop
    $ /apps/game/rel/game/bin/game stop


