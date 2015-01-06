-module(account).

-include("session.hrl").
-include("account_pb.hrl").
-include("account.hrl").
-include("role_pb.hrl").
-include("role.hrl").
-include("error_code.hrl").

-export([handle/1]).

handle({CsAccountLogin, Session}) when erlang:is_record(CsAccountLogin, cs_account_login) ->
    #cs_account_login{account_id=AccountId} = CsAccountLogin,
    #session{connector_pid=ConnectorPid} = Session,
    {ok, RiakConnectionPid} = establish_riak_connection(),
    Key = erlang:integer_to_binary(AccountId),
    ResponseRecord = 
        case riakc_pb_socket:get(RiakConnectionPid, {<<"player">>,<<"account">>},Key) of
            {ok, _AccountObject} ->
                start_role_process(Session),
                #sc_account_login{ result = ?SUCCESS };
            {error, _ } ->
                #sc_account_login{ result = ?ROLE_NOT_EXISTED }
        end,
    connector:send_to_role( ConnectorPid, ResponseRecord ),
    NewSession = Session#session{account_id=AccountId,riak_connection_pid=RiakConnectionPid},
    {ok, NewSession};

handle({CsAccountCreateRole,Session}) when erlang:is_record(CsAccountCreateRole, cs_account_create_role) ->
    #session{account_id = AccountId, connector_pid=ConnectorPid, riak_connection_pid = RiakConnectionPid } = Session,
    #cs_account_create_role{name = RoleName,sex = RoleSex} = CsAccountCreateRole,
    %%generate a new random role id
    RoleRecord = #role{role_id=undefined,account_id=AccountId,name=RoleName,sex=RoleSex,level=1,diamond=20},
    RoleObject = riakc_obj:new({<<"player">>, <<"role">>},undefined,erlang:term_to_binary(RoleRecord)),
    {ok, StoredRoleObject } = riakc_pb_socket:put(RiakConnectionPid,RoleObject),
    RoleKey = riakc_obj:key(StoredRoleObject),
    AccountRecord = #account{ account_id = AccountId, role_id = RoleKey},
    AccountKey = erlang:integer_to_binary(AccountId),
    _AccountObject = riakc_obj:new({<<"player">>,<<"account">>}, AccountKey, erlang:term_to_binary(AccountRecord) ),
    ResponseRecord = #sc_account_create_role{result=?SUCCESS, role_id=RoleKey},
    {ok, RolePid} = start_role_process(Session),
    connector:send_to_role(ConnectorPid, ResponseRecord),
    NewSession = #session{role_pid=RolePid, role_id = RoleKey},
    {ok, NewSession}.

establish_riak_connection() ->
    {ok, RiakNodeList} = server_config:get(riak_node_list),
    RandomNode = lists:nth(random:uniform(erlang:length(RiakNodeList)),RiakNodeList),
    {Ip, Port} = RandomNode,
    {ok, RiakConnectionPid } = riakc_pb_socket:start_link(Ip,Port),
    {ok, RiakConnectionPid }.

start_role_process(_Session) ->
    ok.