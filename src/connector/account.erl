-module(account).

-include("session.hrl").
-include("account_pb.hrl").
-include("account.hrl").
-include("role_pb.hrl").
-include("role.hrl").
-include("error_code.hrl").

-export([handle/1]).

handle({CsAccountLogin, Session}) when erlang:is_record(CsAccountLogin, cs_account_login) ->
    #session{connector_pid=ConnectorPid, session_state=?CONNECTED} = Session,
    #cs_account_login{account_id=AccountId} = CsAccountLogin, %% in production environment, we shall validate or 
                                                              %% fetch account profile from 
                                                              %% another account server using a string token.
                                                              %% for simplicity, just assume that the account id ok.
    {ok, RiakDb} = establish_riak_connection(),
    ResponseRecord = 
        case get_account_record(RiakDb,AccountId) of
            AccountRecord when erlang:is_record(AccountRecord, account)->
                #account{account_id=AccountId, role_id_list=RoleIdList} = AccountRecord,
                #sc_account_login{ result = ?SUCCESS, role_id_list = RoleIdList };
            not_existed ->
                #sc_account_login{ result = ?SUCCESS, role_id_list = [] }
        end,
    connector:send_to_role( ConnectorPid, ResponseRecord ),
    NewSession = Session#session{account_id=AccountId,
                                 riak_db=RiakDb,
                                 session_state=?LOGGED_IN},
    {ok, NewSession};

handle({CsAccountCreateRole,Session}) when erlang:is_record(CsAccountCreateRole, cs_account_create_role) ->
    #session{account_id = AccountId,
             connector_pid=ConnectorPid, 
             riak_db = RiakDb,
             session_state=?LOGGED_IN} = Session,
    #cs_account_create_role{account_id = AccountId, name = RoleName,sex = RoleSex} = CsAccountCreateRole,
    RoleRecord = #role{role_id=undefined,account_id=AccountId,name=RoleName,sex=RoleSex,level=1,diamond=20},
    RoleObject = riakc_obj:new(<<"role">>,undefined,erlang:term_to_binary(RoleRecord)),
    {ok, StoredRoleObject} = riakc_pb_socket:put(RiakDb,RoleObject),
    RoleKey = riakc_obj:key(StoredRoleObject),
    AccountRecord = 
        case get_account_record(Session) of
            AR when erlang:is_record(AR, account) -> AR;
            not_existed -> #account{account_id=AccountId,role_id_list=[]}
        end,
    AccountRoleKeys = AccountRecord#account.role_id_list,
    NewAccountRoleIdList = [RoleKey|AccountRoleKeys],
    NewAccountRecord = #account{account_id = AccountId, role_id_list = NewAccountRoleIdList},
    AccountObject = riakc_obj:new(<<"account">>, 
                                  erlang:integer_to_binary(AccountId), 
                                  erlang:term_to_binary(NewAccountRecord)),
    riakc_pb_socket:put(RiakDb,AccountObject),
    ResponseRecord = #sc_account_create_role{result=?SUCCESS, role_id_list=NewAccountRoleIdList},
    connector:send_to_role(ConnectorPid, ResponseRecord),
    {ok, Session};

handle({CsAccountGetRole,Session}) when erlang:is_record(CsAccountGetRole,cs_account_get_role) ->
    #cs_account_get_role{role_id=RoleId} = CsAccountGetRole,
    RoleIdBinary = erlang:iolist_to_binary(RoleId),
    #session{account_id=_AccountId, riak_db=RiakDb,connector_pid=ConnectorPid, session_state=?LOGGED_IN} = Session,
    AccountRecord = get_account_record(Session),
    RoleIdList = AccountRecord#account.role_id_list,
    ScAccountGetRole = 
        case lists:member(RoleIdBinary, RoleIdList) of
            false ->
                #sc_account_get_role{result=?ROLE_NOT_YOURS};
            true ->
                case riakc_pb_socket:get(RiakDb,<<"role">>,RoleIdBinary) of
                    {ok, RoleObject} ->
                        RoleObjectValue = riakc_obj:get_value(RoleObject),
                        RoleRecord = erlang:binary_to_term(RoleObjectValue),
                        #role{name=Name,sex=Sex,level=Level} = RoleRecord,
                        RoleInfo = #role_info{role_id=RoleIdBinary,name=Name,sex=Sex,level=Level},
                        #sc_account_get_role{result=?SUCCESS,role=RoleInfo};
                    _ ->
                        #sc_account_get_role{result=?ROLE_NOT_EXISTED}
                end
        end,
    connector:send_to_role(ConnectorPid,ScAccountGetRole),
    ok.
    
    
    

establish_riak_connection() ->
    RiakNodeList = [{"127.0.0.1", 12001},{"127.0.0.1",12002}],%%todo get riak node list from configuration.
    RandomNode = lists:nth(random:uniform(erlang:length(RiakNodeList)),RiakNodeList),
    {Ip, Port} = RandomNode,
    {ok, RiakConnectionPid } = riakc_pb_socket:start_link(Ip,Port),
    {ok, RiakConnectionPid }.

get_account_record(Session) when erlang:is_record(Session,session) ->
    #session{account_id=AccountId,riak_db=RiakDb} = Session,
    get_account_record(RiakDb,AccountId).
get_account_record(RiakDb, AccountId) when erlang:is_integer(AccountId) ->
    AccountIdBinary = erlang:integer_to_binary(AccountId),
    case riakc_pb_socket:get(RiakDb,<<"account">>,AccountIdBinary) of
        {ok, AccountObject} ->
            AccountObjectValue = riakc_obj:get_value(AccountObject),
            AccountRecord = erlang:binary_to_term(AccountObjectValue),
            AccountRecord;
        {error, _ } ->
            not_existed
    end.



%% start_role_process(Session) when erlang:is_record(Session,session) ->
%% #session{role_id=RoleId}=Session,
%% case global:whereis_name(RoleId) of
%% Pid when erlang:is_pid(Pid) ->
%% erlang:send(Pid, reconnected), %%TODO send new session pid here, connector pid may change.
%% lager:info("global role process still exists with Pid:~p, use this one to hanle reconnection",[Pid]),
%% ok;
%% undefined ->
%% GameNodeList = ['server1_game1@127.0.0.1' ], %server_config:get(game_node_list),
%% GameNode = lists:nth( random:uniform(erlang:length(GameNodeList)), GameNodeList),
%% ok = rpc:call(GameNode, role, start_role, [Session]),
%% lager:info("Role Process Started with RoleId:~p",[RoleId]),
%% ok
%% end.