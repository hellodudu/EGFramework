-module(gateway_protocol).
-include("gateway.hrl").

-behaviour(gen_server).
-behaviour(ranch_protocol).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([init/4]).
-export([start_link/4]).
-record(state, {socket,transport}).


%%api functions
message(PlayerId, Message) ->
    GateWayName = "gateway_" ++ erlang:integer_to_list(PlayerId),
    GateWay = erlang:list_to_atom(GateWayName),
    case erlang:whereis(GateWay) of
        Pid when erlang:is_pid(Pid) -> Pid ! {message, Message};
        _Port when erlang:is_port(_Port) -> ignore;
        undefined -> ignore
    end.

%%Behavior functions
start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init([]) -> {ok, not_used}.

init(Ref, Socket, Transport, _Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport}, ?TIMEOUT).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp,Socket,Data},State=#state{socket=Socket,transport=Transport}) ->
    try
        Record = decode(Data),
        handle(Record),
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
handle_info({message, Record}, State) when is_tuple(Record) ->
    RecordNameAtom = erlang:element(1, Record),
    RecordName = erlang:atom_to_list(RecordNameAtom),
    RecordNameBinary = erlang:list_to_binary(RecordName),
    [ "sc", ProtoFileName | _Command ] = string:tokens(RecordName, "_"),
    File = erlang:list_to_atom(ProtoFileName ++ "_pb"),
    SerializedData = File:encode(Record),
    RecordNameLength = erlang:length(RecordName),
    RecordNameLengthBinary = binary:encode_unsigned(RecordNameLength, little),
    RepliedIOData = << RecordNameLengthBinary/binary, RecordNameBinary/binary, SerializedData/binary >>,
    #state{socket=Socket, transport=Transport} = State,
    Transport:send(Socket, RepliedIOData).

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
    File = erlang:list_to_atom(ProtoFileName ++ "_pb"),
    MessageRecord = erlang:list_to_atom(string:to_lower(MessageString)),
    File:decode(MessageRecord, Rest2).


%%All Client Message Handled Here
handle(#cs_account_anonymous_login{device_token=DeviceToken}) ->
    account:get_account(anonymous,DeviceToken).


