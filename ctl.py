#!/usr/bin/env python
from __future__ import print_function

import os
import json
import sys
import string
import random


erl = "erl "
f = file( "config/server.json" )
s = json.load(f)
f.close()
serverId = s["server_id"]
cookie = s["cookie"]

def get_dependencies():
    ebinList = [ 'ebin/' ]
    ebinStringPipe = os.popen( 'find deps -type d | grep ebin' )
    for line in ebinStringPipe:
        ebinList.append( line.strip('\n') )
    dependencyDirectory = ' '
    for e in ebinList:
        dependencyDirectory += (' '+ e)
    return dependencyDirectory

dependencies = get_dependencies()

def build():
    command = erl + '-pa ' + dependencies + ' -make'
    os.system( command )
    command = '''for file in $(find src -type f -name "*.app.src"); 
                  do 
                      fileBasename=$(basename ${file}); 
                      cp -av ${file} ebin/${fileBasename%.*}; 
                  done
              '''
    os.system( command )


def proto():
    command = erl + '-pa ' + dependencies + ' -eval \'proto:compile_all()\''
    os.system(command)

def rebuild():
    command = ''' rm -fr ebin && mkdir ebin '''
    os.system( command )
    proto()
    build()

def debug():
    command = erl+'-pa '+dependencies+' -name debug'+str(int(random.uniform(1,20000)))+'@127.0.0.1'+' -setcookie '+cookie+" -hidden"
    os.system( command )
        
def start_connectors():
    connectorNodes = s['connector']
    for node in connectorNodes:
        id = node['id']
        host = node['host']
        port = node['port']
        nodeName = serverId+'_'+id+'@'+host
        command1 = erl + '-setcookie ' + cookie + " -s lager" + " -pa " + dependencies + " -detached -name " + nodeName
        command2 = " -port " + str(port) + " -eval \'application:ensure_all_started(connector).\' "
        command = command1+command2
        os.system(command)

def start_games():
    gameNodes = s['game']
    for node in gameNodes:
        id = node['id']
        host = node['host']
        nodeName = serverId+'_'+id+'@'+host
        command1 = erl + '-setcookie ' + cookie + " -s lager" +" -pa " + dependencies + " -detached -name " + nodeName
        command2 = " -eval \'application:start(game).\'"
        command = command1 + command2
        os.system(command)

def start():
    start_connectors()
    start_games() 

if __name__ == '__main__':
    try: 
        command = sys.argv[1]
        if command == 'build':
            build()
        elif command == 'start':
            start()
        elif command == 'debug':
            debug()
        elif command == 'stop':
            stop()
        elif command == 'rebuild':
            rebuild()
        elif command == 'proto':
            proto()
        else:
            print( "illegal command: " + command )
    except IndexError:
        print( "illegal command. ")
        exit(1)
