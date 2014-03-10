%%Author: Anthony Jiang <2nth0nyj@gmail.com>
%% -*- coding: utf-8 -*-
-module(account).

-include("account_pb.hrl").
-include("role_pb.hrl").

-export([account_login/1]).


account_login(CsAccountLogin) ->
    #cs_account_login{name=Name, password=Password} = CsAccountLogin,
    Sql = "SELECT `id` FROM `game`.`account` WHERE `name` = "++ "'" ++ 
              Name ++ "' " ++ "AND" ++ "`password` =" ++ "'" ++Password++"'" ++" ;",
    ResultPacket = emysql:execute(game, Sql),
    AccountRecords = emysql:as_record(ResultPacket, account, record_info(fields,account)),
    case AccountRecords of
        [] -> {error, -1}; %%login error, no such account
        [AccountRecord] -> 
            AccountId = AccountRecord#account.id,
            AccountIdString = erlang:integer_to_list(AccountId),
            Sql1 = "SELECT `role`.* FROM `account` JOIN `account_to_role` ON
                    `account`.`id` = `account_to_role`.`account_id`  JOIN `role` ON `account_to_role`.`role_id` = `role`.`id` WHERE `account`.`id` = " ++ "'" ++
                       AccountIdString ++ "' ;",
            ResultPacket1 = emysql:execute(game, Sql1),
            RoleRecords = emysql:as_record(ResultPacket1, role, record_info(fields,role)),
            {ok, RoleRecords}
    end.


