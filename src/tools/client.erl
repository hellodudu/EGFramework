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
-export([send/1]).
-export([stop/0]).

start(Ip,Port) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Ip,Port}, []).
start_link(Ip,Port) ->
    gen_server:start_link({local, ?MODULE},?MODULE,{Ip,Port},[]).

send(MessageRecord) ->
    erlang:send( ?MODULE, {to_server,MessageRecord}).

stop() ->
    erlang:send(?MODULE, stop).

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
    lager:info("Sending Record ~p, with IOData = ~p ",[Record,IOData]),
    gen_tcp:send(Socket,IOData),
    {noreply, Socket};
handle_info({tcp,_Socket,Data}, Socket) ->
    {Module, Response} = codec:decode(Data),
    lager:info("Received response: ~p", [Module,Response]),
    {noreply, Socket};
handle_info({tcp_closed,_Socket}, Socket) ->
    lager:info("Client disconnected"),
    {stop,normal,ok};
handle_info({tcp_error, Socket, Reason}, Socket) ->
    lager:error("connection disconnected with error reason: ~p",[Reason]),
    {noreply,Socket};
handle_info(stop,Socket) ->
    ok = gen_tcp:close(Socket),
    {stop, normal,ok};
handle_info(_Unhandled, Socket) ->
    lager:error( "Unhandled Info: ~p",[_Unhandled]),
    {noreply,Socket}.

terminate(_Reason,_Socket) ->
    ok.

code_change(_Vsn, Socket, _Extra) ->
    {ok, Socket}.


    