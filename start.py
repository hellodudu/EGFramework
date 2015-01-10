#!/usr/bin/env python
from __future__ import print_function

import os
import json
import sys


erl = "erl "

def get_dependencies():
    ebinList = [ '\'ebin\'' ]
    ebinStringPipe = os.popen( 'find deps -type d | grep ebin' )
    for line in ebinStringPipe:
        ebinList.append( line.strip('\n') )
    dependencyDirectory = ' '
    for e in ebinList:
        dependencyDirectory += (' '+ e)
    return dependencyDirectory

def build():
    dependencyDirectory = get_dependencies()
    command = erl + '-pz ' + dependencyDirectory + ' -make'
    os.system( command )
    command = '''for file in $(find src -type f -name "*.app.src"); 
                  do 
                      fileBasename=$(basename ${file}); 
                      cp -av ${file} ebin/${fileBasename%.*}; 
                  done
              '''
    os.system( command )

def debug():
    dependencyDirectory = get_dependencies()
    command = erl + '-pz ' + dependencyDirectory + ' -name debug@127.0.0.1'
    os.system( command )


def start():
    f = file("config/server.json")
    s = json.load(f)
    f.close()
    serverId = s["server_id"]
    cookie = s["cookie"]

####start connectors
    connectorNodes = s['connector']
    dependencies = get_dependencies()
    for node in connectorNodes:
        id = node["id"]
        host = node["host"]
        port = node["port"]
        command1 = erl + "-pa " + dependencies + " -detached -name '" + serverId + "_" + id + "@" + host + "\'"
        command2 = "\'" + " -eval ok = application:ensure_all_started(connector) \'" + " -port " + str(port)
        command = command1 + command2  
        os.system(command)
####start game nodes
    gameNodes = s["game"]
    for node in gameNodes:
        id = node['id']
        host = node['host']
        command1 = erl + "-pa " + dependencies + " -detached -name '" + serverId + "_" + id + "@" + host + "\'"
        command2 = "\'" + " -eval ok = application:start(game) \'"
        command = command1 + command2
        os.system(command) 


if __name__ == '__main__':
    try: 
        command = sys.argv[1]
        if command == 'build':
            build()
        elif command == 'start':
            start()
        elif command == 'debug':
            debug()
        else:
            print( "illegal command: " + command )
    except IndexError:
        print( "illegal command. ")
        exit(1)
