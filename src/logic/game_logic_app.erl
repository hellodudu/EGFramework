%%Author: Anthony Jiang <2nth0nyj@gmail.com>
%% -*- coding: utf-8 -*-

-module(game_logic_app).
-behaviour(application).
-export([start/2, stop/1]).

-export([]).

start(_Type, _StartArgs) ->
    case game_logic_sup:start_link(StartArgs) of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.

stop(State) ->
    ok.


