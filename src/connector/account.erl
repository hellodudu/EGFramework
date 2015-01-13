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
    #cs_account_login{account_id=AccountId} = CsAccountLogin,
    {ok, RiakConnectionPid} = establish_riak_connection(),
    Key = erlang:integer_to_binary(AccountId),
    ResponseRecord = 
        case riakc_pb_socket:get(RiakConnectionPid, {<<"player">>,<<"account">>},Key) of
            {ok, _AccountObject} ->
                {ok, RolePid} = start_role_process(Session),
                #sc_account_login{ result = ?SUCCESS };
            {error, _ } ->
                RolePid = undefined,
                #sc_account_login{ result = ?ROLE_NOT_EXISTED }
        end,
    connector:send_to_role( ConnectorPid, ResponseRecord ),
    NewSession = Session#session{account_id=AccountId,
                                 role_pid=RolePid,
                                 riak_connection_pid=RiakConnectionPid,
                                 session_state=?LOGGED_IN},
    {ok, NewSession};

handle({CsAccountCreateRole,Session}) when erlang:is_record(CsAccountCreateRole, cs_account_create_role) ->
    #session{account_id = AccountId, connector_pid=ConnectorPid, riak_connection_pid = RiakConnectionPid } = Session,
    #cs_account_create_role{name = RoleName,sex = RoleSex} = CsAccountCreateRole,
    RoleRecord = #role{role_id=undefined,account_id=AccountId,name=RoleName,sex=RoleSex,level=1,diamond=20},
    RoleObject = riakc_obj:new({<<"player">>, <<"role">>},undefined,erlang:term_to_binary(RoleRecord)),
    {ok, StoredRoleObject } = riakc_pb_socket:put(RiakConnectionPid,RoleObject),
    RoleKey = riakc_obj:key(StoredRoleObject),
    AccountRecord = #account{ account_id = AccountId, role_id = RoleKey},
    AccountKey = erlang:integer_to_binary(AccountId),
    _AccountObject = riakc_obj:new({<<"player">>,<<"account">>}, AccountKey, erlang:term_to_binary(AccountRecord) ),
    ResponseRecord = #sc_account_create_role{result=?SUCCESS, role_id=RoleKey},
    NewSession1 = Session#session{role_id=RoleKey},
    {ok, RolePid} = start_role_process(NewSession1),
    connector:send_to_role(ConnectorPid, ResponseRecord),
    NewSession2 = NewSession1#session{role_pid=RolePid},
    {ok, NewSession2}.

establish_riak_connection() ->
    %%todo get riak node list from configuration.
    RiakNodeList = [{"127.0.0.1", 12001},{"127.0.0.1",12002}],
    RandomNode = lists:nth(random:uniform(erlang:length(RiakNodeList)),RiakNodeList),
    {Ip, Port} = RandomNode,
    {ok, RiakConnectionPid } = riakc_pb_socket:start_link(Ip,Port),
    {ok, RiakConnectionPid }.

start_role_process(Session) when erlang:is_record(Session,session) ->
    #session{role_id=RoleId}=Session,
    case global:whereis_name(RoleId) of
        Pid when erlang:is_pid(Pid) ->
            {ok, Pid};
        undefined ->
            GameNodeList = server_config:get(game_node_list),
            GameNode = lists:nth( random:uniform(erlang:length(GameNodeList)), GameNodeList),
            {ok, RolePid} = rpc:call(GameNode, role, start_role, [Session]),
            {ok, RolePid}
    end.