-module(role_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/0]).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,{{one_for_one,10,10}, []}}.
