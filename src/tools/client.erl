%% simulate a client to connect to server.
-module(client).

-include("account_pb.hrl").
-include("account.hrl").
-include("connector.hrl").
-include("error_code.hrl").
-include("role_pb.hrl").
-include("role.hrl").
-include("session.hrl").

-behavoir(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start/2,start_link/2]).

start(Ip,Port) ->
    gen_server:start(?MODULE, {Ip,Port}, []).
start_link(Ip,Port) ->
    gen_server:start_link(?MODULE,{Ip,Port},[]).

init({Ip,Port}) ->
    SocketOptions = [{packet,2},{keepalive,true},{active,true}],
    {ok, Socket} = gen_tcp:connect(Ip, Port, SocketOptions),
    {ok, Socket}.

handle_call(get_socket, _From, Socket) ->
    {reply,Socket,Socket};
handle_call(_,_From,Socket)->
    {reply, ok, Socket}.
handle_cast(_Req,Socket) ->
    {noreply, Socket}.

handle_info({to_server,Record},Socket) ->
    IOData = codec:encode( Record ),
    gen_tcp:send(Socket,IOData);
handle_info({tcp,_Socket,Data}, _Socket) ->
    {Module, Response} = codec:decode(Data),
    lager:info("Received response: ~p", [Module,Response]);
handle_info({tcp_closed,_Socket}, _Socket) ->
    lager:info("Client disconnected");
handle_info({tcp_error, Socket, Reason}, Socket) ->
    lager:error("connection disconnected with error reason: ~p",[Reason]);
handle_info(_Unhandled, _Socket) ->
    lager:error( "Unhandled Info: ~p",[_Unhandled]).

terminate(_Reason,_Socket) ->
    ok.

code_change(_Vsn, Socket, _Extra) ->
    {ok, Socket}.


    