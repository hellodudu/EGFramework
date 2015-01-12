-module(user_default).

-include("account_pb.hrl").
-include("account.hrl").
-include("connector.hrl").
-include("error_code.hrl").
-include("role_pb.hrl").
-include("role.hrl").
-include("session.hrl").

-compile([export_all]).

get_meta() ->
    user_default:module_info().