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

-compile([export_all]).

start(Ip,Port) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Ip,Port}, []).
start_link(Ip,Port) ->
    gen_server:start_link({local, ?MODULE},?MODULE,{Ip,Port},[]).

send(MessageRecord) ->
    erlang:send( ?MODULE, {to_server,MessageRecord}).

stop() ->
    erlang:send(?MODULE, stop).

init({Ip,Port}) ->
    SocketOptions = [{packet,raw},{keepalive,true},{active,false}],
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
    IODataLength = erlang:byte_size(IOData),
    SocketData = << IODataLength:1/big-unsigned-integer-unit:16, IOData/binary >>,
    lager:debug("Sending Record ~p ",[Record]),
    gen_tcp:send(Socket,SocketData),
    erlang:send(erlang:self(),recv),
    {noreply, Socket};
handle_info({tcp,_Socket,Data}, Socket) ->
    {Module, Response} = codec:decode(Data),
    lager:debug("Received response: ~p", [Module,Response]),
    {noreply, Socket};
handle_info({tcp_closed,_Socket}, _Socket) ->
    lager:debug("Client disconnected"),
    {stop,normal,socket_disconnected};
handle_info({tcp_error, Socket, Reason}, Socket) ->
    lager:debug("connection disconnected with error reason: ~p",[Reason]),
    {noreply,Socket};
handle_info(stop,Socket) ->
    ok = gen_tcp:close(Socket),
    {stop, normal,ok};
handle_info(recv, Socket) ->
    case gen_tcp:recv(Socket, 2, timer:seconds(5)) of
        {ok, Packet1} -> 
            << PacketLength:1/big-unsigned-integer-unit:16 >> = erlang:iolist_to_binary(Packet1),
            case gen_tcp:recv(Socket,PacketLength,timer:seconds(5)) of
                {ok, Packet2} ->
                    {_Module, Record} = codec:decode(erlang:iolist_to_binary(Packet2)),
                    lager:debug("Client Received Record:~p",[Record]),
                    {noreply, Socket};
                {error,Reason} ->
                    lager:error( "Client receiving error with reason:~p", [Reason]),
                    {stop,normal,timeout}
            end;
        {error, Reason} ->
            lager:error( "Client receiving error with reason:~p", [Reason]),
            {stop,normal,timeout}
    end;
handle_info(_Unhandled, Socket) ->
    lager:error( "Unhandled Info: ~p",[_Unhandled]),
    {noreply,Socket}.

terminate(_Reason,_Socket) ->
    ok.

code_change(_Vsn, Socket, _Extra) ->
    {ok, Socket}.


    