-module(connector_app).
-behaviour(application).

-include("connector.hrl").

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    {ok,[[PortString]]} = init:get_argument(port),
    Port = erlang:list_to_integer(PortString),
    pass = case ranch:start_listener(connector, 
                                   2000, 
                                   ranch_tcp, 
                                   [{port,Port},
                                    {active, once}, 
                                    {packet,2},
                                    {reuseaddr,true},
                                    {keepalive,true},
                                    {max_connections,256}], 
                                   connector, 
                                   []) of
        {ok,_ } -> pass;
        {error, {already_started, _}} -> pass;
        RanchError -> lager:error("Cannot start ranch with Error: ~p",RanchError)
    end,

    Ret = case connector_sup:start_link() of
        {ok, Pid} ->
            lager:info("Connector on node ~p, listening on port ~p", [erlang:node(),Port]),
            {ok, Pid};
        Error ->
            Error
    end,
    
    role_mgr:init(),
    Ret.

stop(State) ->
    lager:info( "Connector stopped at node ~p", [erlang:node()]),
    State.


