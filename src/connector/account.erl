-module(account).

-include("account_pb.hrl").
-include("role_pb.hrl").
-include("error_code.hrl").

-export([handle/1]).

handle(CsAccountLogin) when erlang:is_record(CsAccountLogin, cs_account_login) ->
    #cs_account_login{token=_Token} = CsAccountLogin,
    %%%%%%% make a http request to game platform to validate login and account info,
    %%%%%%% for simplicity, generate a random account id here,
    %%%%%%% for production environment, should get an account from game platform by a http reqeust
    _AccountId = random:uniform(20000),
    ReturnedRecord = #sc_account_login{ result = ?SUCCESS },
    gateway:send( ReturnedRecord ).

