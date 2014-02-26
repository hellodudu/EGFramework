-module(gateway_app).
-behaviour(application).

-include("gateway.hrl").

-export([start/2, stop/1]).

%% -spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
%% 	{ok, Pid :: pid()}
%% 	| {ok, Pid :: pid(), State :: term()}
%% 	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
    {ok, _} = ranch:start_listener(gateway, 
                                   200, 
                                   ranch_tcp, 
                                   [{port, ?GAME_PORT},{active, once}], 
                                   gateway_protocol, 
                                   []),
    case gateway_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.


%% -spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(State) ->
    State.


