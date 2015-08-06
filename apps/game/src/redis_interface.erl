-module(redis_interface).

-include("role.hrl").

-export([
         load_role/1,
         save_role/2
        ]).

%% 读取角色
load_role(RoleID) ->
    RetRoleRecord = #role{},
    RoleID1 = redis_query:get(io_lib:format("role:~B:role_id", [RoleID])),
    AccountID = redis_query:get(io_lib:format("role:~B:account_id", [RoleID])),
    Name = redis_query:get(io_lib:format("role:~B:name", [RoleID])),
    Sex = redis_query:get(io_lib:format("role:~B:sex", [RoleID])),
    Level = redis_query:get(io_lib:format("role:~B:level", [RoleID])),
    Diamond = redis_query:get(io_lib:format("role:~B:diamond", [RoleID])),
    RetRoleRecord1 = RetRoleRecord#role{role_id = RoleID1, account_id = AccountID, name = Name, 
                                        sex = Sex, level = Level, diamond = Diamond},

    lager:info("redis load_role result = ~p~n", [RetRoleRecord1]),
    RetRoleRecord.

save_role(RoleID, RoleRec) ->
    redis_query:set(io_lib:format("role:~B:role_id", [RoleID]), RoleRec#role.role_id),
    redis_query:set(io_lib:format("role:~B:account_id", [RoleID]), RoleRec#role.account_id),
    redis_query:set(io_lib:format("role:~B:name", [RoleID]), RoleRec#role.account_id),
    redis_query:set(io_lib:format("role:~B:sex", [RoleID]), RoleRec#role.sex),
    redis_query:set(io_lib:format("role:~B:level", [RoleID]), RoleRec#role.level),
    redis_query:set(io_lib:format("role:~B:diamond", [RoleID]), RoleRec#role.diamond),
    true.
