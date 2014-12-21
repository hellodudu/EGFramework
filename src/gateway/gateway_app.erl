%%Author: Anthony Jiang <2nth0nyj@gmail.com>
%% -*- coding: utf-8 -*-
-module(gateway_app).
-behaviour(application).

-include("gateway.hrl").

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    {ok, _} = ranch:start_listener(gateway, 
                                   2000, 
                                   ranch_tcp, 
                                   [{port, ?GAME_PORT},{active, once}, {packet,2}], 
                                   gateway, 
                                   []),
    case gateway_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.

stop(State) ->
    State.


