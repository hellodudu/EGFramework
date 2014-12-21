%%Author: Anthony Jiang <2nth0nyj@gmail.com>
%% -*- coding: utf-8 -*-
-module(game_logic_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/1]).
start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = {'role_sup',{'role_sup',start_link,[]},
	      permanent,2000,supervisor,['role_sup']},
    {ok,{{one_for_all,10,10}, [AChild]}}.

