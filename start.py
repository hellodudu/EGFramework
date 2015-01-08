#!/usr/bin/env python
from __future__ import print_function

import os
import json
import sys


erl = "erl "

def build():
    os.system( "erl -make" )
    
def start_riak():
    pass

def start():
    f = file("config/server.json")
    s = json.load(f)
    f.close()
    serverId = s["server_id"]
    cookie = s["cookie"]

####start connectors
    connectorNodes = s['connector']
    for node in connectorNodes:
        id = node["id"]
        host = node["host"]
        port = node["port"]
        command1 = erl + " -name '" + serverId + "_" + id + "@" + host + "\'" + " -pz ebin -pz deps/*/ebin "
        command2 = "\'" + " -eval ok = application:ensure_all_started(connector) \'" + " -port " + str(port)
        command = command1 + command2  
        os.system(command)
####start game nodes
    gameNodes = s["game"]
    for node in gameNodes:
        id = node['id']
        host = node['host']
        command1 = erl + " -name '" + serverId + "_" + id + "@" + host + "\'" + " -pz ebin -pz deps/*/ebin "
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
        else:
            print( "illegal command: " + command )
    except IndexError:
        print( "illegal command. ")
        exit(1)
