-module(role_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/1]).

start_link(RoleRecord) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, [RoleRecord]) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

init([RoleRecord]) ->
    RoleSupervisor = {role, {role, start_link, [RoleRecord]}, permanent, 2000, supervisor, [role]},
    {ok,{{one_for_one,10,10}, [RoleSupervisor]}}.
