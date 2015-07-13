-module(game_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RoleSupervisor = {role_mgr, {role_mgr, init, []}, permanent, 2000, worker, [role_mgr]},
    {ok, {{one_for_one, 10, 10}, [RoleSupervisor]}}.
