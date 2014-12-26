#start erlang game server


erl -detached -name connector@127.0.0.1 -pz deps/ranch/ebin deps/erlang_protobuffs/ebin ebin/ -eval 'application:ensure_all_started(connector).'