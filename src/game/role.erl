-module(role).
-behaviour(gen_server).

-include("session.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1]).
-export([start_role/1,stop_role/1]).
-export([restart_role/1]).

start_role(Session) when erlang:is_record(Session, session) ->
    RoleId = Session#session.role_id,
    RoleChildSpec = {RoleId,
                     {role, start_link, [Session]},
                     transient,
                     2000,
                     worker,
                     [role]
                     },
    case supervisor:start_child(role_sup, RoleChildSpec) of
        {ok, ChildPid} when erlang:is_pid(ChildPid) -> 
            ok;
        {ok, ChildPid,_ChildInfo} when erlang:is_pid(ChildPid) -> 
            ok;
        {error,{already_started,ChildPid}} when erlang:is_pid(ChildPid) -> 
            erlang:send(ChildPid, reconnected),
            ok;
        {error,already_present} -> 
            supervisor:delete_child(role_sup, RoleId),
            start_role(Session)
    end.
    %%supervisor is locally registered, player logic process is globally registered.

stop_role(RoleId) ->
    supervisor:terminate_child(role_sup, RoleId),
    supervisor:delete_child(role_sup, RoleId).

restart_role(RoleId) ->
    supervisor:restart_child(role_sup, RoleId).

start_link(Session) when erlang:is_record(Session, session)->
    RoleId = Session#session.role_id,
    gen_server:start_link({global, RoleId}, ?MODULE, Session, []).

init([]) ->
    {ok, not_used};
init(Session) when erlang:is_record(Session, session) ->
    #session{connector_pid=ConnectorPid} = Session,
    erlang:monitor(process, ConnectorPid),
    {ok, Session};
init(Other) ->
    {ok, Other}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reconnected, Session) ->
    TimerRef = erlang:erase(timer_ref),
    erlang:cancel_timer(TimerRef),
    {noreply, Session};

handle_info(connector_down, Session) ->
    {stop, normal, Session};

handle_info({'DOWN',_MonitorRef,_Type, _Object, _Info},Session) ->
    TimerRef = erlang:send_after(timer:seconds(30), erlang:self(), connector_down),
    erlang:put(timer_ref, TimerRef),
    {noreply,Session};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason,_State) ->
    %% recycle session resource here.
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok, State}.
