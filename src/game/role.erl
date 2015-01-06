-module(role).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, not_used}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info(MessageRecord, State) ->
%%     try
%%         MessageName = erlang:element(1, erlang:element(1, MessageRecord) ),
%%         [ "cs", ModuleName, _Command ] = MessageName,
%%         Module = erlang:list_to_existing_atom(ModuleName),
%%         Module:handle( MessageRecord )
%%     catch
%%         ErrorType:ErrorReason ->
%%             {noreply, State}
%%     end,
%%     {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok, State}.

%% handle(CsRoleGetInfo) when erlang:is_record(CsRoleGetInfo, cs_role_get_info) ->
%%     AccountId = CsRoleGetInfo#cs_get_role_info.account_id,
%%     ScRoleGetInfo = 
%%         case db:get_role_info(AccountId) of
%%             [] -> 
%%                 #sc_role_get_info{ result = ?ROLE_NOT_EXISTED };
%%             [AccountRoleInfo] ->
%%                 Role = #role{ role_id=0, name="",level=1 }
%%                        #sc_role_get_info{ result = ?SUCCESS, role=Role}
%%         end,
%%     gateway:send_to_role(ScRoleGetInfo).
