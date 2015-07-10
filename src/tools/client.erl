%% simulate a client to connect to server.
-module(client).

-include("../../include/account_pb.hrl").
-include("../../include/connector.hrl").
-include("../../include/error_code.hrl").
-include("../../include/role_pb.hrl").
-include("../../include/role.hrl").
-include("../../include/session.hrl").

-behavoir(gen_server).

-compile([export_all]).

start(Ip,Port) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Ip,Port}, []).
start_link(Ip,Port) ->
    gen_server:start_link({local, ?MODULE},?MODULE,{Ip,Port},[]).

send(MessageRecord) ->
    erlang:send( ?MODULE, {to_server,MessageRecord}).

send_login() ->
    Login = #cs_account_login{account_id=1},
    io:format("login account = ~p~n", [Login#cs_account_login.account_id]),
    send(Login).

stop() ->
    erlang:send(?MODULE, stop).

init({Ip,Port}) ->
    SocketOptions = [binary, {packet,4}, {keepalive,true}, {active,false}],
    case gen_tcp:connect(Ip, Port, SocketOptions) of
        {ok, Socket} ->
            io:format("connect success!~n"),
            {ok, Socket};
        Other ->
            io:format("connect failed with reason:~p~n", [Other]),
            {error, Other}
    end.

handle_call(get_socket, _From, Socket) ->
    {reply,Socket,Socket};
handle_call(_,_From,Socket)->
    {reply, ok, Socket}.
handle_cast(_Req,Socket) ->
    {noreply, Socket}.

handle_info({to_server,Record},Socket) ->
    IOData = lib_codec:encode( Record ),
    IODataLength = erlang:byte_size(IOData),
    SocketData = << IODataLength:1/big-unsigned-integer-unit:16, IOData/binary >>,
    lager:debug("Sending Record ~p ",[Record]),
    gen_tcp:send(Socket,SocketData),
    erlang:send(erlang:self(),recv),
    {noreply, Socket};
handle_info({tcp,_Socket,Data}, Socket) ->
    {Module, Response} = lib_codec:decode(Data),
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
                    {_Module, Record} = lib_codec:decode(erlang:iolist_to_binary(Packet2)),
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


    
