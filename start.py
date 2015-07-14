#!/usr/bin/env python
from __future__ import print_function

import os
import sys
import subprocess

HOSTNAME = "127.0.0.1"
PORT     = "2345"
USERNAME = "root"
PASSWORD = "123qwe"
SERVERID = ""
COOKIE   = "server"

erl = "erl "

def get_deps():
    '''
    get all deps
    '''
    command = 'rebar get-deps'
    os.system( command )

def get_proto_files_list():
    files = os.listdir("proto")
    def f(x):
        s = x.split('.')
        if len(s) == 1:
            return False
        else:
            filename, extension = s
            if extension == "proto":
                return True
            else:
                return False
    return filter(f, files)

def generate_pb_list():
    files = get_proto_files_list()
    r = ""
    for file in files:
        file = file.split('.')
        name,extension = file
        pb_name = (name+"_pb")
        load_pb = "code:load_file("+pb_name+")"
        r += ( name + "," +load_pb+"," )
    return r

pb_list = generate_pb_list()


def get_dep_ebin_dirs():
    '''
    get all deps' ebin directories
    '''
    ebinList = [ 'ebin/' ]
    ebinStringPipe = os.popen( 'find deps -type d | grep ebin' )
    for line in ebinStringPipe:
        ebinList.append( line.strip('\n') )
    dependencyDirectory = ' '
    for e in ebinList:
        dependencyDirectory += (' '+ e)
    return dependencyDirectory

dep_ebin_dirs = get_dep_ebin_dirs()

def get_apps_ebin_dirs():
    '''
    get all apps' ebin directories
    '''
    ebinList = [ 'ebin/' ]
    ebinStringPipe = os.popen( 'find apps -type d | grep ebin' )
    for line in ebinStringPipe:
        ebinList.append( line.strip('\n') )
    dependencyDirectory = ' '
    for e in ebinList:
        dependencyDirectory += (' '+ e)
    return dependencyDirectory

apps_ebin_dirs = get_apps_ebin_dirs()

def build_src_files():
    r = ""
    for root, dirs, files in os.walk("src", True):
        for name in files:
            filename, extension = name.split('.')
            if extension == "erl":
                r += (" " + os.path.join(root, name))
    return r


def build():
    '''
    run "rabar compile" directly
    '''
    command = 'rebar compile'
    os.system( command )

def proto():
    '''
    proceed .proto files to generate _pb.erl and _pb.hrl files
    '''
    command = "erlc -I src/tools/ -o ebin/ src/tools/proto.erl"
    subprocess.call(command, shell=True)

    command = erl + ' -pa ' + dep_ebin_dirs + ' -eval \'proto:compile_all()\''
    subprocess.call(command, shell=True)

def rebuild():
    '''the first time run "rebar compile" will get some error like "cannot find files", just pass it'''
    get_deps()
    subprocess.call('rebar compile', shell=True)

    '''proto() needs module(protobuffs) that generated by "rebar compile"'''
    proto()

    '''after run proto(), it will generate some *_pb.hrl and *_pb.erl files, run "rebar compile" again'''
    subprocess.call('rebar compile', shell=True)

    '''build lib/ proto/ test/ tools/'''
    src_list = build_src_files()
    command = "erlc -o ebin/ " + src_list
    os.system( command )

def get_db_game_sql():
    '''
     load src/tools/db_game.sql
    '''
    f = open('src/tools/db_game.sql')
    sql_text = ''
    try:
        sql_text = f.read()
    finally:
        f.close()
    return sql_text

db_sql = get_db_game_sql()

def reset_db():
    command = 'mysql -u' + USERNAME + ' -h' + HOSTNAME + ' -p' + PORT + ' -p123qwe -e '
    command += '"'
    command += db_sql
    command += '"'
    os.system( command )
    print('reset success!')

def start_game(detach=False):
    nodeName = SERVERID + 'game' + '@' + HOSTNAME
    command1 = erl + '-setcookie ' + COOKIE
    if detach == True:
        command1 += " -detached"
    command2 = " -s lager" +" -pa " + dep_ebin_dirs + " " + apps_ebin_dirs + " -name " + nodeName
    command3 = " -port " + PORT + " -eval \'" + pb_list+ "application:ensure_all_started(game).\'"
    command = command1 + command2 + command3
    os.system(command)

def start_db(detach=False):
    nodeName = SERVERID + 'db' + '@' + HOSTNAME
    command1 = erl + '-setcookie ' + COOKIE
    if detach == True:
        command1 += " -detached"
    command2 = " -s lager" + " -pa " + dep_ebin_dirs + " " + apps_ebin_dirs + " -name " + nodeName
    command3 = " -port" + PORT + " -eval \'" + pb_list+ "application:ensure_all_started(db_session).\'"
    command = command1 + command2 + command3
    os.system(command)

def start_client():
    '''
    start client
    '''
    #nodeName = 'client' + '@' + HOSTNAME
    command1 = erl + '-setcookie ' + COOKIE + " -s lager" + " -pa " + dep_ebin_dirs + " " + apps_ebin_dirs
    command2 = " -port " + PORT + " -host " + HOSTNAME + " -eval \'" + pb_list+ " application:ensure_all_started(client).\' "
    command = command1+command2
    os.system(command)


def start():
    start_db(True)
    start_game(True)

def start_release():
    command1 = "apps/db_session/rel/db_session/bin/db_session start" + " -port " + PORT + " -host " + HOSTNAME
    command2 = "apps/game/rel/game/bin/game start" + " -port " + PORT + " -host " + HOSTNAME
    os.system(command1)
    os.system(command2)

def stop_release():
    command1 = "apps/db_session/rel/db_session/bin/db_session stop"
    command2 = "apps/game/rel/game/bin/game stop"
    os.system(command1)
    os.system(command2)

def stop():
    command = "ps aux|grep ensure_all_started|awk '{print $2}'|xargs kill -9"
    os.system(command)

def generate():
    subprocess.call('rebar compile generate', shell=True)

def clean():
    os.system("rebar clean")

def help():
    helpMessage = '''
        Execute this script to generate protocol files,
        build, and start the erlang game server.
        Usage:
        ./start build #get all deps and build all project files
        ./start rebuild #get all deps and generate *_pb.hrl files, then build all project files
        ./start reset_db #reset mysql and import init sql files with src/tools/db_game.sql
        ./start start #start this game server
        ./stop  kill all node process
        '''
    print(helpMessage)

if __name__ == '__main__':
    commandFunMapper = { 'build':build, 'start':start, 'start_release':start_release, 'stop':stop, 'generate':generate,
                        'start_client':start_client, 'start_game':start_game, 'start_db':start_db, 'reset_db':reset_db,
                        'rebuild':rebuild, 'proto':proto, 'stop_release':stop_release, 'clean':clean, 'help':help }
    try:
        command = sys.argv[1]
        commandFunMapper[command]()
    except IndexError:
        commandFunMapper['help']()
        exit(1)
    except KeyError:
        commandFunMapper['help']()
        exit(1)


