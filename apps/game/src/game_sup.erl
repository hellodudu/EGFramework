-module(game_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    role_mgr:init(),
    RoleSupervisor = {role_sup,{role_sup,start_link,[]},
	       permanent,2000,supervisor,[role_sup]},
    {ok,{{one_for_one,10,10}, [RoleSupervisor]}}.
