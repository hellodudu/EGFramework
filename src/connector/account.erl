-module(account).

-include("session.hrl").
-include("account_pb.hrl").
-include("account.hrl").
-include("role_pb.hrl").
-include("role.hrl").
-include("error_code.hrl").

-export([handle/1]).

handle({CsAccountLogin, Session}) when erlang:is_record(CsAccountLogin, cs_account_login) ->
    #session{session_state=?CONNECTED} = Session,
    #cs_account_login{account_id=AccountId} = CsAccountLogin, %% in production environment, we shall validate or 
                                                              %% fetch account profile from 
                                                              %% another account server using a string token.
                                                              %% for simplicity, just assume that the account id is ok here.
    {ok, RiakDb} = establish_riak_connection(),
    NewSession = Session#session{account_id=AccountId,riak_db=RiakDb,session_state=?LOGGED_IN},
    connector:send_to_role(Session, #sc_account_login{result=?SUCCESS}),
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
    RoleId = riakc_obj:key(StoredRoleObject),
    RoleObjectWithKey = riakc_obj:update_value(StoredRoleObject,RoleRecord#role{role_id=RoleId}),
    riakc_pb_socket:put(RiakDb,RoleObjectWithKey),
    AccountRecord = 
        case get_account_record(Session) of
            AccountRecord1 when erlang:is_record(AccountRecord1, account) -> AccountRecord1;
            not_existed -> #account{account_id=AccountId,role_id_list=[]}
        end,
    AccountRoleKeys = AccountRecord#account.role_id_list,
    NewAccountRoleIdList = [RoleId|AccountRoleKeys],
    NewAccountRecord = #account{account_id = AccountId, role_id_list = NewAccountRoleIdList},
    AccountObject = riakc_obj:new(<<"account">>, 
                                  erlang:integer_to_binary(AccountId), 
                                  erlang:term_to_binary(NewAccountRecord)),
    riakc_pb_socket:put(RiakDb,AccountObject),
    ResponseRecord = #sc_account_create_role{result=?SUCCESS, role_id_list=NewAccountRoleIdList},
    connector:send_to_role(ConnectorPid, ResponseRecord),
    {ok, Session};

handle({CsAccountGetRoleIdList, Session}) when erlang:is_record(CsAccountGetRoleIdList,cs_account_get_role_id_list) ->
    #session{session_state=?LOGGED_IN} = Session,
    ScAccountGetRoleIdList = 
        case get_account_record(Session) of
            AccountRecord when erlang:is_record(AccountRecord, account) -> 
                RoleIdList = AccountRecord#account.role_id_list,
                #sc_account_get_role_id_list{result=?SUCCESS,role_id_list=RoleIdList};
            _ -> 
                #sc_account_get_role_id_list{result=?ROLE_NOT_EXISTED}
        end,
    connector:send_to_role(Session, ScAccountGetRoleIdList),
    ok;

handle({CsAccountGetRole,Session}) when erlang:is_record(CsAccountGetRole,cs_account_get_role) ->
    #cs_account_get_role{role_id=RoleId} = CsAccountGetRole,
    RoleIdBinary = erlang:iolist_to_binary(RoleId),
    #session{account_id=_AccountId, riak_db=RiakDb,session_state=?LOGGED_IN} = Session,
    AccountRecord = get_account_record(Session),
    RoleIdList = AccountRecord#account.role_id_list,
    ScAccountGetRole = 
        case lists:member(RoleIdBinary, RoleIdList) of
            false ->
                #sc_account_get_role{result=?ROLE_NOT_YOURS};
            true ->
                case get_role_record(RiakDb,RoleIdBinary) of
                    RoleRecord when erlang:is_record(RoleRecord,role) ->
                        #role{name=Name,sex=Sex,level=Level} = RoleRecord,
                        RoleInfo = #role_info{role_id=RoleIdBinary,name=Name,sex=Sex,level=Level},
                        #sc_account_get_role{result=?SUCCESS,role=RoleInfo};
                    _ ->
                        #sc_account_get_role{result=?ROLE_NOT_EXISTED}
                end
        end,
    connector:send_to_role(Session,ScAccountGetRole),
    ok;

handle({CsAccountEnterGame, Session}) when erlang:is_record(CsAccountEnterGame,cs_account_enter_game) ->
    #cs_account_enter_game{role_id=RoleId} = CsAccountEnterGame,
    #session{riak_db=RiakDb, session_state=?LOGGED_IN} = Session,
    RoleIdBinary = erlang:iolist_to_binary(RoleId),
    AccountRecord = get_account_record(Session),
    RoleIdList = AccountRecord#account.role_id_list,
    ScAccountEnterGame = 
        case lists:member(RoleIdBinary,RoleIdList) of
            false ->
                #sc_account_enter_game{result = ?ROLE_NOT_YOURS};
            true ->
                case get_role_record(RiakDb,RoleIdBinary) of
                    RoleRecord when erlang:is_record(RoleRecord,role)->
                        SessionRoleRecord = RoleRecord#role{session=Session},
                        case role:start_role(SessionRoleRecord) of
                            ok -> #sc_account_enter_game{result=?SUCCESS};
                            _ -> #sc_account_enter_game{result=?SERVER_INTERNAL_ERROR}
                        end;
                    _ ->
                        #sc_account_enter_game{result=?ROLE_NOT_EXISTED}
                end
        end,
    Result = ScAccountEnterGame#sc_account_enter_game.result,
    NewSession = 
        case Result of
            ?SUCCESS -> Session#session{role_id=RoleIdBinary};
            _ -> Session
        end,
    connector:send_to_role(Session, ScAccountEnterGame),
    {ok, NewSession}.

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

get_role_record(RiakDb,RoleId) ->
    RoleIdBinary = erlang:iolist_to_binary(RoleId),
    case riakc_pb_socket:get(RiakDb, <<"role">>, RoleIdBinary) of
        {ok, RoleObject} ->
            RoleObjectValue = riakc_obj:get_value(RoleObject),
            RoleRecord = erlang:binary_to_term(RoleObjectValue),
            RoleRecord;
        {error, _} ->
            not_existed
    end.
