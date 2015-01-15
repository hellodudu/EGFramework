-module(connector).
-include("connector.hrl").
-include("account_pb.hrl").
-include("role_pb.hrl").
-include("session.hrl").

-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([init/4, start_link/4]).
-export([send_to_role/2]).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

send_to_role(ConnectorPid, Response) when erlang:is_pid(ConnectorPid) ->
    erlang:send( ConnectorPid, {response, Response} );
send_to_role(ConnectorPid, {response, Response}) when erlang:is_pid(ConnectorPid)->
    erlang:send( ConnectorPid, {response, Response} ).

init([]) -> {ok, #session{} }.

init(Ref, Socket, Transport, _Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once},{packet,2},{keepalive,true}]),
    ConnectorPid = erlang:self(),
    erlang:put(connector_pid, ConnectorPid),
    Session = #session{socket=Socket,
                       transport=Transport,
                       connector_pid=ConnectorPid,
                       session_state=?CONNECTED},
    gen_server:enter_loop(?MODULE, [], Session, ?SESSION_TIMEOUT).

handle_call(_Request, _From, Session) ->
    Reply = ok,
    {reply, Reply, Session}.

handle_cast(_Msg, Session) ->
    {noreply, Session}.

handle_info({tcp,Socket,Data},Session) ->
    try
        #session{socket=Socket, transport=Transport,connector_pid=ConnectorPid} = Session,
        BinaryData = erlang:iolist_to_binary(Data),
        {Module, RequestRecord} = codec:decode(BinaryData),
        lager:debug("Decoded Module:~p, RequestRecord:~p", [Module, RequestRecord]),
        NewSession1 = 
            case route({Module,RequestRecord},Session) of
                {ok, NewSession} when erlang:is_record(NewSession, session) ->
                    NewSession;
                _ ->
                    Session
            end,
        Transport:setopts(Socket, [{active,once},{packet,2},{keepalive,true}]),
        {noreply, NewSession1, ?SESSION_TIMEOUT}
    catch
        Error:Reason ->
            lager:error( "Error in connector while handling message with 
                                       Error:~w, Reason:~w, and stacktrace: ~w",
                                      [ Error, Reason, erlang:get_stacktrace()] ),
            {noreply, Session, ?SESSION_TIMEOUT}
    end;
handle_info({tcp_closed,_Socket}, Session) ->
    after_session_lost(Session),
    {stop, normal, Session};
handle_info({tcp_error,_Socket, Reason}, Session) ->
    after_session_lost(Session),
    {stop, Reason, Session};
handle_info(timeout, Session) ->
    after_session_lost(Session),
    {stop, normal, Session};
handle_info({response, Record}, Session) when erlang:is_tuple(Record) ->
    #session{socket=Socket, transport=Transport} = Session,
    IOData = codec:encode(Record),
    lager:debug("Server Response Record:~p",[Record]),
    Transport:send(Socket, IOData),
    {noreply, Session};
handle_info(_Info, Session) ->
    {noreply, Session}.

terminate(_Reason, Session) ->
    lager:debug("Session:~p TERMINATED!", [Session]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

route({Module,RequestRecord}, Session) ->
    %% first try to route to player process,
    %% if failed, handle it here in connector process.
    #session{connector_pid=ConnectorPid, role_id=RoleId} = Session,
    RolePid = global:whereis_name(RoleId),
    if
        erlang:is_pid(RolePid) -> erlang:send(RolePid, {RequestRecord, Session});
        true ->
            if
                erlang:is_pid(ConnectorPid) -> Module:handle({RequestRecord, Session});
                true -> erlang:error( no_process_to_handle_this_request )
            end
    end.

after_session_lost(Session) ->
    RoleId = Session#session.role_id,
    RolePid = global:whereis_name(RoleId),
    if
        erlang:is_pid( RolePid ) -> erlang:send( RolePid, connection_lost );
        true -> ignore
    end.
