-module(game_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RoleMgr = {role_mgr, {role_mgr, init, []}, permanent, 2000, worker, [role_mgr]},
    RoleSupervisor = {role_sup, {role_sup, start_link, []}, permanent, 2000, supervisor, [role_sup]},
    {ok, {{one_for_one, 10, 10}, [RoleMgr, RoleSupervisor]}}.
