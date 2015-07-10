%% simulate a client to connect to server.
-module(client).

-include("../../include/account_pb.hrl").
-include("../../include/connector.hrl").
-include("../../include/error_code.hrl").
-include("../../include/role_pb.hrl").
-include("../../include/role.hrl").
-include("../../include/session.hrl").

-behavoir(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2]).
-export([start/2, stop/0, do_recv/1]).

% 发送操作消息到服务器
-export([
        send_login/0,
        send_create_role/1
    ]).

start(Ip, Port) ->
    gen_server:start({local, ?MODULE}, ?MODULE, {Ip,Port}, []).

start_link(Ip, Port) ->
    gen_server:start_link({local, ?MODULE},?MODULE,{Ip,Port},[]).

send(MessageRecord) ->
    erlang:send( ?MODULE, {to_server,MessageRecord}).

send_login() ->
    Login = #cs_account_login{account_id=1},
    send(Login).

send_create_role(Name) ->
    CreateRole = #cs_account_create_role{account_id=1, name=Name, sex=1},
    send(CreateRole).

stop() ->
    erlang:send(?MODULE, stop).

init({Ip,Port}) ->
    SocketOptions = [binary, {packet, 0}, {active, false}, {keepalive, true}],
    case gen_tcp:connect(Ip, Port, SocketOptions) of
        {ok, Socket} ->
            io:format("connect success!~n"),
            %spawn(fun() -> do_recv(Socket) end),
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

handle_info({to_server, Record},Socket) ->
    BinaryData = erlang:term_to_binary(Record),
    gen_tcp:send(Socket, BinaryData),
    erlang:send(erlang:self(), recv),
    {noreply, Socket};
handle_info({tcp,_Socket,Data}, Socket) ->
    %{Module, Response} = lib_codec:decode(Data),
    RecordData = erlang:binary_to_list(Data),
    lager:info("Received response: ~p", [RecordData]),
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

% 处理收到的服务器消息
handle_info(recv, Socket) ->
    case gen_tcp:recv(Socket, 0, timer:seconds(5)) of
        {ok, Packet1} -> 
            <<Rest/binary>> = Packet1,
            Record = binary_to_term(Rest),
            lager:info("recv RespondRec = ~p", [Record]),
            {noreply, Socket};
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

do_recv(Socket) ->
    lager:info("into do_recv"),
    receive
        {tcp, Socket, Data} ->
            Val = binary_to_term(Data),
            lager:info("recv server respond = ~p", [Val]),
            do_recv(Socket);
        {tcp_closed, Socket} ->
            lager:info("server socket closed!");
        Other ->
            lager:info("other = ~p", [Other])
    end.
