-module(connector_app).
-behaviour(application).

-include("connector.hrl").

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    {ok,[[PortString]]} = init:get_argument(port),
    Port = erlang:list_to_integer(PortString),
    {ok, _} = ranch:start_listener(connector, 
                                   2000, 
                                   ranch_tcp, 
                                   [{port,Port},{active, once}, {packet,2},{reuseaddr,true}], 
                                   connector, 
                                   []),
    case connector_sup:start_link() of
        {ok, Pid} ->
            lager:info("connector started on node: ~p with listening port: ~p", [erlang:node(), Port]),
            {ok, Pid};
        Error ->
            Error
    end.

stop(State) ->
    State.


