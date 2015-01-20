#!/usr/bin/env python
from __future__ import print_function

import os
import json
import sys
import string
import random
import commands


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
    files = get_proto_files_list()
    list.sort(files)
    protoStartNum = 20000
    nextProtoStep = 100;
    output = ""
    protoNum = protoStartNum
    resultList = []
    for file in files:
        module,extension = file.split('.')
        if extension == "proto":
            command = '''./proto/proto_mapper --proto_path=./proto --erlang_mapper_output=./ --proto_id='''+str(protoNum) + " proto/"+file
            output = commands.getoutput(command)
            resultList.append(output)
            protoNum += nextProtoStep
    lastProto = resultList[-1]
    lastSemicolonIndex = lastProto.rfind(';')
    lastProtoWithoutSemicolon = lastProto[:lastSemicolonIndex]
    lastProto = lastProtoWithoutSemicolon + '.'
    resultList[-1] = lastProto
    fileHeaders = '-module(proto_mapper).\n-export([get/1]).\n'
    fileContents = '\n'.join(resultList)
    fileContents = fileHeaders + fileContents
    with open( "src/proto/proto_mapper.erl", "w") as f:
        f.write(fileContents)
    command = erl + '-pa ' + dependencies + ' -eval \'proto:compile_all()\''
    os.system(command)

def rebuild():
    command = ''' rm -fr ebin && mkdir ebin '''
    os.system( command )
    proto()
    build()

def debug():
    command = erl+'-pa '+dependencies+' -name debug'+str(int(random.uniform(1,20000)))+'@127.0.0.1'+' -setcookie '+cookie+" -hidden" + " -eval \'" + pb_list + "ok\'"
    os.system( command )
        
def start_connectors():
    connectorNodes = s['connector']
    for node in connectorNodes:
        id = node['id']
        host = node['host']
        port = node['port']
        nodeName = serverId+'_'+id+'@'+host
        command1 = erl + '-setcookie ' + cookie + " -s lager" + " -pa " + dependencies + " -detached -name " + nodeName
        command2 = " -port " + str(port) + " -eval \'" + pb_list+ " application:ensure_all_started(connector).\' "
        command = command1+command2
        os.system(command)

def start_games():
    gameNodes = s['game']
    for node in gameNodes:
        id = node['id']
        host = node['host']
        nodeName = serverId+'_'+id+'@'+host
        command1 = erl + '-setcookie ' + cookie + " -s lager" +" -pa " + dependencies + " -detached -name " + nodeName
        command2 = " -eval \'" + pb_list+ "application:start(game).\'"
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
