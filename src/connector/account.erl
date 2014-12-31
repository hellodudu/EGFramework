-module(account).

-include("account_pb.hrl").
-include("role_pb.hrl").
-include("error_code.hrl").

-export([handle/1]).

handle(CsAccountLogin) when erlang:is_record(CsAccountLogin, cs_account_login) ->
    #cs_account_login{account_id=AccountId} = CsAccountLogin,
    DbConnection = establish_db_session(),
    Bucket = <<"account">>,
    Key = erlang:integer_to_binary(AccountId),
    {ok,AccountObject} = DbConnection:get(Bucket, Key, 1 ),
    ReturnedRecord = 
        case raik_object:value_count( AccountObject ) of
            0 -> 
                #sc_account_login{ result = ?ROLE_NOT_EXISTED };
            _ ->
                #sc_account_login{ result = ?SUCCESS }
        end,
    connector:send( ReturnedRecord ),
    erlang:send( erlang:self(), {new_db_connection,DbConnection} ).

handle(CsAccountCreateRole) when erlang:is_record(CsAccountCreateRole, cs_account_create_role) ->
    

establish_db_session() ->
    {ok, DbNodeList} = application:get_env(connector, db_node_list),
    RandomNode = lists:nth( random:uniform( erlang:length(DbNodeList) ), DbNodeList ),
    {ok, DbConnection } = riak:client_connect(RamdomNode),
    DbConnection.