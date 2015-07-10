-module(role).
-behaviour(gen_server).

-include("../../include/session.hrl").
-include("../../include/role.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

start_link(RoleRecord) when erlang:is_record(RoleRecord, role)->
    RoleId = RoleRecord#role.role_id,
    gen_server:start_link({global, RoleId}, ?MODULE, RoleRecord , []).

init(RoleRecord) when erlang:is_record(RoleRecord, role) ->
    #role{role_id=RoleId, session=Session} = RoleRecord,
    #session{connector_pid=ConnectorPid} = Session,
    erlang:monitor(process,ConnectorPid),
    NewSession = Session#session{role_id=RoleId},
    NewRoleRecord = RoleRecord#role{session=NewSession},
    {ok, NewRoleRecord}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Module, Request,_Session}, Role) ->
    try
        case Module:handle({Request,Role}) of
            {ok,NewRole} -> {noreply, NewRole};
            _ -> {noreply, Role}
        end
    catch
        Error:Reason ->
            lager:error("Error ~p while handling:~p with Reason:~p stacktrace:~p",
                        [Error, {Module,Request},Reason, erlang:get_stacktrace()]),
            {noreply, Role}
    end;

handle_info(reconnected, Role) ->
    TimerRef = Role#role.reconnectionTimer,
    erlang:cancel_timer(TimerRef),
    {noreply, Role#role{reconnectionTimer=undefined}};

handle_info(connector_down, Role) ->
    prepare_to_stop_role(Role),
    {stop, normal, Role};

handle_info(connection_lost, Role) ->
    prepare_to_stop_role(Role),
    {stop, normal, Role};

handle_info({'DOWN',_MonitorRef,_Type, _Object, _Info},Role) ->
    TimerRef = erlang:send_after(timer:seconds(30), erlang:self(), connector_down),
    {noreply,Role#role{reconnectionTimer=TimerRef}};

handle_info(stop, Role) ->
    {stop, normal, Role};

handle_info(_Info, Role) ->
    {noreply, Role}.

terminate(normal,Role) ->
    %% recycle session resource here.
    #role{role_id=RoleId} = Role,
    case RoleId of
        undefined -> pass;
        _Else -> 
            RoleIdBinary = erlang:iolist_to_binary(RoleId),
            supervisor:delete_child(role_sup,RoleIdBinary)
    end,
    ok;
terminate(_Reason,_Role) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok, State}.


prepare_to_stop_role(_Role) -> ok.
