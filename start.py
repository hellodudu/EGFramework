#!/usr/bin/env python
from __future__ import print_function

import os
import sys
import random

HOSTNAME = "127.0.0.1"
PORT     = "3306"
USERNAME = "root"
PASSWORD = "123qwe"
SERVERID = "server1"
COOKIE   = "server1"

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

def build():
    '''
    run "rabar compile" directly
    '''
    get_deps()
    command = 'rebar compile'
    os.system( command )



    #command = erl + '-pa ' + dep_ebin_dirs + ' -make'
    #os.system( command )
    #command = '''for file in $(find src -type f -name "*.app.src");
                  #do
                      #fileBasename=$(basename ${file});
                      #cp -av ${file} ebin/${fileBasename%.*};
                  #done
              #'''
    #os.system( command )


def proto():
    '''
    proceed .proto files to generate _pb.erl and _pb.hrl files
    '''
    command = erl + '-pa ' + dep_ebin_dirs + ' -eval \'proto:compile_all()\''
    os.system(command)

def rebuild():
    '''
     delete all files in ebin/ and make new ebin/*.beam files
    '''
    command = ''' rm -fr ebin && mkdir ebin '''
    os.system( command )
    build()
    proto()
    build()

def debug():
    command = erl+'-pa '+dep_ebin_dirs+' -name debug'+str(int(random.uniform(1,20000)))+'@127.0.0.1'+' -setcookie '+COOKIE+" -hidden" + " -eval \'" + pb_list + "ok\'"
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

def start_connectors():
    connectorNodes = s['connector']
    for node in connectorNodes:
        id = node['id']
        host = node['host']
        port = node['port']
        nodeName = SERVERID+'_'+id+'@'+host
        command1 = erl + '-setcookie ' + COOKIE + " -s lager" + " -pa " + dep_ebin_dirs + " -detached -name " + nodeName
        command2 = " -port " + str(port) + " -eval \'" + pb_list+ " application:ensure_all_started(connector).\' "
        command = command1+command2
        os.system(command)

def start_games():
    gameNodes = s['game']
    for node in gameNodes:
        id = node['id']
        host = node['host']
        nodeName = SERVERID+'_'+id+'@'+host
        command1 = erl + '-setcookie ' + COOKIE + " -s lager" +" -pa " + dep_ebin_dirs + " -detached -name " + nodeName
        command2 = " -eval \'" + pb_list+ "application:start(game).\'"
        command = command1 + command2
        os.system(command)

def start():
    start_connectors()
    start_games()

def help():
    helpMessage = '''
        Execute this script to generate protocol files,
        build, and start the erlang game server.
        Usage:
        ./ctl build #build *.erl to *.beam files
        ./ctl proto #generate *.proto to *.erl and *.hrl files
        ./ctl start #start this game server
        '''
    print(helpMessage)

if __name__ == '__main__':
    commandFunMapper = { 'build':build, 'start':start, 'debug':debug, 'reset_db':reset_db,
        'rebuild':rebuild, 'proto':proto,'help':help }
    try:
        command = sys.argv[1]
        commandFunMapper[command]()
    except IndexError:
        commandFunMapper['help']()
        exit(1)
    except KeyError:
        commandFunMapper['help']()
        exit(1)


