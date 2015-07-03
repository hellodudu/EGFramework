-module(role_mgr).
-include("role.hrl").
-include("session.hrl").
-include("error_code.hrl").
-include("db.hrl").
-include("role_pb.hrl").
-include("account_pb.hrl").

-export([start_role/1, stop_role/1]).
-export([restart_role/1, start_role/2]).
-export([init/0]).
-export([handle/1]).

%% 初始化
init() ->
    put(online_role_list, []).


%% 玩家进入游戏世界 开启role进程
start_role(RoleRecord) when erlang:is_record(RoleRecord, role) ->
    #role{role_id=RoleId} = RoleRecord,
    case global:whereis_name(RoleId) of
        Pid when erlang:is_pid(Pid) -> 
            DataTree = get(role_data),
            case gb_trees:lookup(RoleId, DataTree) of
                {value, _RoleValue} ->
                    gb_trees:update(RoleId, 1, DataTree),
                    put(role_data, DataTree);
                none ->
                    gb_trees:insert(RoleId, 1, DataTree)
            end,
            erlang:send(Pid,reconnected),
            ok;
        undefined ->
            GameNodeList = [ 'server1_game1@127.0.0.1' ], %%todo, get game node list from configuration.
            GameNode = lists:nth( random:uniform(erlang:length(GameNodeList)), GameNodeList),
            rpc:call(GameNode, role_mgr, start_role, [from_connector,RoleRecord])
    end.

%% 重连
start_role(from_connector,RoleRecord) when erlang:is_record(RoleRecord,role)->
    RoleId = RoleRecord#role.role_id,
    RoleChildSpec = {RoleId,
                     {role, start_link, [RoleRecord]},
                     transient,
                     2000,
                     worker,
                     [role]
                    },
    StartChildResult = supervisor:start_child(role_sup, RoleChildSpec),
    case StartChildResult of
        {ok, ChildPid} when erlang:is_pid(ChildPid) -> 
            ok;
        {ok, ChildPid,_ChildInfo} when erlang:is_pid(ChildPid) -> 
            ok;
        {error,{already_started,ChildPid}} when erlang:is_pid(ChildPid) -> 
            erlang:send(ChildPid, reconnected),
            ok;
        {error,already_present} -> 
            supervisor:delete_child(role_sup, RoleId),
            start_role(RoleRecord)
    end.
    %%supervisor is locally registered, player logic process is globally registered.

%% 结束玩家进程
stop_role(RoleRecord) when erlang:is_record(RoleRecord, role) ->
    #role{role_id=RoleId} = RoleRecord,
    stop_role(RoleId);
stop_role(RoleId) ->
    supervisor:terminate_child(role_sup, RoleId),
    supervisor:delete_child(role_sup, RoleId).

restart_role(RoleId) ->
    supervisor:restart_child(role_sup, RoleId).

%% 未登录玩家处理
handle({CsAccountLogin, Session}) when erlang:is_record(CsAccountLogin, cs_account_login) ->
    #session{session_state=?CONNECTED} = Session,
    #cs_account_login{account_id=AccountId} = CsAccountLogin, %% in production environment, we shall validate or 
                                                              %% fetch account profile from 
                                                              %% another account server using a string token.
                                                              %% for simplicity, just assume that the account id is ok here.
    global:register_name(AccountId, erlang:self()),            %% register account id as connector identifier
    NewSession = Session#session{account_id=AccountId,
                                 session_state=?LOGGED_IN},
    connector:send_to_role(Session, #sc_account_login{result=?SUCCESS}),
    {ok, NewSession};

handle({CsAccountCreateRole,Session}) when erlang:is_record(CsAccountCreateRole, cs_account_create_role) ->
    #session{account_id = AccountId, session_state=?LOGGED_IN} = Session,
    OnlineRoleList = get(online_role_list),

    ScAccountCreateRole =
        case (erlang:length(OnlineRoleList) < ?ROLE_MAX_NUM_CREATED) of
            true ->
                #cs_account_create_role{account_id = AccountId, 
                                        name = RoleName,
                                        sex = RoleSex} = CsAccountCreateRole,
                RoleRecord = #role{role_id=undefined,
                                   account_id=AccountId,
                                   name=RoleName,
                                   sex=RoleSex,
                                   level=1,
                                   diamond=20},

                %% id简单自增1
                RoleId = db_session:get_max_rolenum() + 1,
                StoreRoleRec = RoleRecord#role{role_id = RoleId},
                case ets:lookup(ets_role, RoleId) of
                    [] ->
                        %% 更新ets数据
                        ets:insert(ets_role, StoreRoleRec),

                        %% 执行sql语句
                        Query = io_lib:format("update role set account_id=~B, name=\"~s\", sex=~B, level=~B, diamond=~B where role_id=~B", 
                            [StoreRoleRec#role.account_id, StoreRoleRec#role.name, StoreRoleRec#role.sex, StoreRoleRec#role.level, StoreRoleRec#role.diamond, StoreRoleRec#role.role_id]),
                        emysql:execute(?DBPOOL, Query),
                        io:format("role insert ok!~n");
                    _ ->
                        io:format("role exist!~n"),
                        ignore
                end,

                %% 更新在线玩家列表
                NewOnlineRoleList = [RoleId|OnlineRoleList],
                put(online_role_list, NewOnlineRoleList),
                #sc_account_create_role{result=?SUCCESS, role_id_list=NewOnlineRoleList};
            false ->
                #sc_account_create_role{result=?ROLE_MAX_NUM_CREATED}
        end,
    connector:send_to_role(Session, ScAccountCreateRole),
    {ok, Session};

handle({CsAccountGetRoleIdList, Session}) when erlang:is_record(CsAccountGetRoleIdList,cs_account_get_role_id_list) ->
    #session{session_state=?LOGGED_IN} = Session,
    ScAccountGetRoleIdList = #sc_account_get_role_id_list{result = ?SUCCESS, role_id_list = get(online_role_list)},
    connector:send_to_role(Session, ScAccountGetRoleIdList),
    ok;

handle({CsAccountGetRole,Session}) when erlang:is_record(CsAccountGetRole,cs_account_get_role) ->
    #cs_account_get_role{role_id=RoleId} = CsAccountGetRole,
    RoleIdBinary = erlang:iolist_to_binary(RoleId),
    #session{account_id=_AccountId, session_state=?LOGGED_IN} = Session,
    RoleIdList = get(online_role_list),
    ScAccountGetRole = 
        case lists:member(RoleIdBinary, RoleIdList) of
            false ->
                #sc_account_get_role{result=?ROLE_NOT_YOURS};
            true ->
                case get_role_record(RoleIdBinary) of
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
    #session{session_state=?LOGGED_IN} = Session,
    RoleIdBinary = erlang:iolist_to_binary(RoleId),
    RoleIdList = get(online_role_list),
    ScAccountEnterGame = 
        case lists:member(RoleIdBinary,RoleIdList) of
            false ->
                #sc_account_enter_game{result = ?ROLE_NOT_YOURS};
            true ->
                case get_role_record(RoleIdBinary) of
                    RoleRecord when erlang:is_record(RoleRecord,role)->
                        SessionRoleRecord = RoleRecord#role{session=Session},
                        case role_mgr:start_role(SessionRoleRecord) of
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

get_role_record(RoleId) ->
    case ets:lookup(ets_role, RoleId) of
        [] ->
            io:format("role doesn't exist!~n"),
            not_existed;
        List when length(List) == 1 ->
            lists:last(List)
    end.

