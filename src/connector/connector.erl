-module(connector).
-include("gateway.hrl").
-include("account_pb.hrl").
-include("role_pb.hrl").
-include("session.hrl").

-behaviour(gen_server).
-behaviour(ranch_protocol).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([init/4, start_link/4]).
-export([role_started/2, send_to_role/1, send_to_role/2]).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

role_started( RoleId, RolePid ) ->
    ConnectorPid = erlang:get(connector_pid),
    erlang:send( ConnectorPid, {role_id, RoleId, role_pid, RolePid} ). 

send_to_role(Message) ->
    ConnectorPid = erlang:get(connector_pid),
    erlang:send( ConnectorPid, Message).
send_to_role(ConnectorPid, Message) when erlang:is_pid(ConnectorPid) ->
    erlang:send( ConnectorPid, {message, Message} );
send_to_role(ConnectorPid, {message, Message}) when erlang:is_pid(ConnectorPid)->
    erlang:send( ConnectorPid, {message, Message} ).

init([]) -> {ok, #session{} }.

init(Ref, Socket, Transport, _Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    ConnectorPid = erlang:self(),
    erlang:put(connector_pid, ConnectorPid),
    Session = #session{socket=Socket,transport=Transport,connector_pid=ConnectorPid},
    gen_server:enter_loop(?MODULE, [], Session, ?TIMEOUT).

handle_call(_Request, _From, Session) ->
    Reply = ok,
    {reply, Reply, Session}.

handle_cast(_Msg, Session) ->
    {noreply, Session}.

handle_info({tcp,Socket,Data},Session) ->
    try
        #session{socket=Socket, transport=Transport,
                 connector_pid=ConnectorPid,role_pid=RolePid} = Session,
        MessageRecord = decode(Data),
        route(MessageRecord,ConnectorPid,RolePid),
        Transport:setopts(Socket, [{active, once}])
    catch
        Error:Reason ->
            error_logger:warning_msg( "Error in connector while decoding message with 
                                       Error:~w, Reason:~w, and stacktrace: ~w",
                                      [ Error, Reason, erlang:get_stacktrace()] )
    end,
    {noreply, Session, ?TIMEOUT};
handle_info({tcp_closed,_Socket}, Session) ->
    after_session_lost(Session),
    {stop, normal, Session};
handle_info({tcp_error,_Socket, Reason}, Session) ->
    after_session_lost(Session),
    {stop, Reason, Session};
handle_info(timeout, Session) ->
    after_session_lost(Session),
    {stop, normal, Session};
handle_info({message, Record}, Session) when erlang:is_tuple(Record) ->
    RecordNameAtom = erlang:element(1, Record),
    RecordName = erlang:atom_to_list(RecordNameAtom),
    RecordNameBinary = erlang:list_to_binary(RecordName),
    [ "sc", ProtoFileName | _Command ] = string:tokens(RecordName, "_"),
    File = erlang:list_to_existing_atom(ProtoFileName ++ "_pb"),
    SerializedData = File:encode(Record),
    RecordNameLength = erlang:length(RecordName),
    RecordNameLengthBinary = binary:encode_unsigned(RecordNameLength, little),
    RepliedIOData = << RecordNameLengthBinary/binary, RecordNameBinary/binary, SerializedData/binary >>,
    #session{socket=Socket, transport=Transport} = Session,
    Transport:send(Socket, RepliedIOData);
handle_info( {role_id, RoleId, role_pid, RolePid}, Session) ->
    NewSession = Session#session{ role_id = RoleId, role_pid=RolePid },
    {noreply, NewSession};
handle_info( {new_db_connection, DbConnection}, Session ) ->
    NewSession = Session#session{db_connection=DbConnection},
    {noreply, NewSession};
handle_info(_Info, Session) ->
    {noreply, Session}.

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

%%route message to gateway process or player process
route(MessageRecord, GatewayPid, undefined) when erlang:is_pid(GatewayPid)->
    MessageName = erlang:element(1, erlang:element(1, MessageRecord) ),
    [ "cs", ModuleName, _Command ] = MessageName,
    Module = erlang:list_to_existing_atom(ModuleName),
    Module:handle( MessageRecord );
route(MessageRecord, _GatewayPid, RolePid) when erlang:is_pid(RolePid) ->
    erlang:send( RolePid, MessageRecord ).

after_session_lost(Session) ->
    RolePid = Session#session.role_pid,
    if
        erlang:is_pid( RolePid ) -> erlang:send( RolePid, connection_lost );
        true -> ignore
    end.
