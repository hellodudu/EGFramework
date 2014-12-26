-module(connector_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    {ok, _} = ranch:start_listener(connector, 
                                   2000, 
                                   ranch_tcp, 
                                   [{port,6666},{active, once}, {packet,2},{reuseaddr,true}], 
                                   connector, 
                                   []),
    case connector_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.

stop(State) ->
    State.


