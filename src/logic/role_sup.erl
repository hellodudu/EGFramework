%% @author JiangAnthony
%% @doc @todo Add description to role_sup.


-module(role_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = {'role',{'role',start_link,[]},
          transient,2000,worker,['role']},
    {ok,{{simple_one_for_one,10,10}, [AChild]}}.
