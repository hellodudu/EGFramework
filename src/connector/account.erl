-module(account).

-include("account_pb.hrl").
-include("role_pb.hrl").
-include("error_code.hrl").

-export([handle/1]).

handle(CsAccountLogin) when erlang:is_record(CsAccountLogin, cs_account_login) ->
    #cs_account_login{account_id=AccountId} = CsAccountLogin,
    {ok, RiakConnectionPid} = establish_riak_connection(),
    Key = erlang:integer_to_binary(AccountId),
    ReturnedRecord = 
        case riakc_pb_socket:get(RiakConnectionPid, <<"account">>, Key) of
            {ok, AccountObject} ->
                #sc_account_login{ result = ?SUCCESS };
            {error, _ } ->
                #sc_account_login{ result = ?ROLE_NOT_EXISTED }
        end,
    connector:send_to_role( ReturnedRecord ),
    erlang:send( erlang:self(), {session_riak_connection_pid,RiakConnectionPid,session_account_id, AccountId}),
    ok.

handle(CsAccountCreateRole) when erlang:is_record(CsAccountCreateRole, cs_account_create_role) ->
    #cs_account_create_role{
                            account_id = AccountId,
                            role_name = RoleName,
                            role_sex = RoleSex} = CsAccountCreateRole,
    %%generate a new role id

establish_riak_connection() ->
    {ok, RiakNodeList} = application:get_env(connector, riak_node_list),
    RandomNode = lists:nth( random:uniform( erlang:length(RiakNodeList) ), RiakNodeList ),
    {Ip, Port} = RandomNode,
    {ok, RiakConnectionPid } = riakc_pb_socket:start_link(Ip,Port),
    {ok, RiakConnectionPid }.