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
    ok = Transport:setopts(Socket, [{active, once}]),
    ConnectorPid = erlang:self(),
    erlang:put(connector_pid, ConnectorPid),
    Session = #session{socket=Socket,transport=Transport,connector_pid=ConnectorPid},
    lager:info("Session Established. Session=~p",[Session]),
    gen_server:enter_loop(?MODULE, [], Session, ?SESSION_TIMEOUT).

handle_call(_Request, _From, Session) ->
    Reply = ok,
    {reply, Reply, Session}.

handle_cast(_Msg, Session) ->
    {noreply, Session}.

handle_info({tcp,Socket,Data},Session) ->
    try
        #session{socket=Socket, transport=Transport} = Session,
        {Module, RequestRecord} = codec:decode(Data),
        lager:info("Connector received module = ~p", [Module]),
        lager:info("Connector received record = ~p",[RequestRecord]),
        NewSession1 = 
            case route({Module,RequestRecord},Session) of
                {ok, NewSession} when erlang:is_record(NewSession, session) ->
                    NewSession;
                _ ->
                    Session
            end,
        Transport:setopts(Socket, [{active, once}]),
        {noreply, NewSession1, ?SESSION_TIMEOUT}
    catch
        Error:Reason ->
            error_logger:warning_msg( "Error in connector while decoding message with 
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
    RepliedIOData = codec:encode(Record),
    Transport:send(Socket, RepliedIOData);
handle_info(_Info, Session) ->
    {noreply, Session}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

route({Module,RequestRecord}, Session) ->
    %% first try to route to player process,
    %% if failed, handle it here in connector process.
    #session{connector_pid=ConnectorPid, role_pid=RolePid} = Session,
    if
        erlang:is_pid(RolePid) -> erlang:send(RolePid, {RequestRecord, Session});
        true ->
            if
                erlang:is_pid(ConnectorPid) -> Module:handle({RequestRecord, Session});
                true -> erlang:error( no_process_to_handle_request )
            end
    end.

after_session_lost(Session) ->
    RolePid = Session#session.role_pid,
    if
        erlang:is_pid( RolePid ) -> erlang:send( RolePid, connection_lost );
        true -> ignore
    end.
