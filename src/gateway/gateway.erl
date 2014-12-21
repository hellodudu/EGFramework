%%Author: Anthony Jiang <2nth0nyj@gmail.com>
%% -*- coding: utf-8 -*-
-module(gateway).
-include("gateway.hrl").
-include("account_pb.hrl").
-include("role_pb.hrl").

-behaviour(gen_server).
-behaviour(ranch_protocol).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([init/4, start_link/4]).
-export([role_started/2, send_to_role/1, send_to_role/2]).

-record(state, {socket=undefined,transport=undefined,
                gateway_pid=undefined,
                role_id=undefined,
                role_pid=undefined}).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

role_started( RoleId, RolePid ) ->
    GatewayPid = erlang:get(gateway_pid),
    erlang:send( GatewayPid, {role_id, RoleId, role_pid, RolePid} ). 

send_to_role(Message) ->
    Gatewaypid = erlang:get(gateway_pid),
    erlang:send( GatewayPid, Message).
send_to_role(GatewayPid, Message) when erlang:is_pid(GatewayPid) ->
    erlang:send( GatewayPid, {message, Message} );
send_to_role(GatewayPid, {message, Message}) when erlang:is_pid(GatewayPid)->
    erlang:send( GatewayPid, {message, Message} ).

init([]) -> {ok, #state{} }.

init(Ref, Socket, Transport, _Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    GatewayPid = erlang:self(),
    erlang:put(gateway_pid, GatewayPid),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport,gateway_pid=GatewayPid}, ?TIMEOUT).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp,Socket,Data},State=#state{socket=Socket,transport=Transport}) ->
    try
        MessageRecord = decode(Data),
        #state{ gateway_pid=GatewayPid,role_pid=RolePid} = State,
        route(MessageRecord,GatewayPid,RolePid),
        Transport:setopts(Socket, [{active, once}])
    catch
        Error:Reason ->
            error_logger:warning_msg( "Error in gateway when decoding message with Error:~w, Reason:~w, and stacktrace: ~w",
                                      [ Error, Reason, erlang:get_stacktrace()] )
    end,
    {noreply, State, ?TIMEOUT};
handle_info({tcp_closed,_Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error,_Socket, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info({message, Record}, State) when erlang:is_tuple(Record) ->
    RecordNameAtom = erlang:element(1, Record),
    RecordName = erlang:atom_to_list(RecordNameAtom),
    RecordNameBinary = erlang:list_to_binary(RecordName),
    [ "sc", ProtoFileName | _Command ] = string:tokens(RecordName, "_"),
    File = erlang:list_to_existing_atom(ProtoFileName ++ "_pb"),
    SerializedData = File:encode(Record),
    RecordNameLength = erlang:length(RecordName),
    RecordNameLengthBinary = binary:encode_unsigned(RecordNameLength, little),
    RepliedIOData = << RecordNameLengthBinary/binary, RecordNameBinary/binary, SerializedData/binary >>,
    #state{socket=Socket, transport=Transport} = State,
    Transport:send(Socket, RepliedIOData);
handle_info( {role_id, RoleId, role_pid, RolePid}, State ) ->
    NewState = State#state{ role_id = RoleId, role_pid=RolePid },
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

decode(BinaryData) when erlang:is_binary(BinaryData) ->
    <<MessageLength:1/little-signed-integer-unit:16, Rest1/binary>> = BinaryData,
    <<Message:MessageLength/bytes, Rest2/binary>> = Rest1,
    MessageString = erlang:binary_to_list(Message),
    ["cs", ProtoFileName | _Command ] = string:tokens( MessageString, "_"),    
    File = erlang:list_to_existing_atom(ProtoFileName ++ "_pb"),
    MessageRecord = erlang:list_to_existing_atom(string:to_lower(MessageString)),
    File:decode(MessageRecord, Rest2).

route(MessageRecord, GatewayPid, undefined) when erlang:is_pid(GatewayPid)->
    MessageName = erlang:element(1, erlang:element(1, MessageRecord) ),
    [ "cs", ModuleName, _Command ] = MessageName,
    Module = erlang:list_to_existing_atom(ModuleName),
    Module:handle( MessageRecord );
route(MessageRecord, _GatewayPid, RolePid) when erlang:is_pid(RolePid) ->
    erlang:send( RolePid, MessageRecord ).