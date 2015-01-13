%%encode/decode protocol buffer reocrd

-module(codec).

-export([decode/1, encode/1]).

encode(Record) when erlang:is_tuple(Record) ->
    RecordNameAtom = erlang:element(1,Record),
    RecordName = erlang:atom_to_list(RecordNameAtom),
    RecordNameBinary = erlang:list_to_binary(RecordName),
    [ _, ProtoFileName | _Command ] = string:tokens(RecordName, "_"),
    File = erlang:list_to_existing_atom(ProtoFileName ++ "_pb"),
    SerializedData = File:encode(Record),
    SerializedDataBinary = erlang:iolist_to_binary(SerializedData),
    RecordNameLength = erlang:length(RecordName),
    RepliedIOData = << RecordNameLength:1/big-unsigned-integer-unit:16, 
                       RecordNameBinary/binary, 
                       SerializedDataBinary/binary >>,
    RepliedIOData.
    

decode(BinaryData) when erlang:is_binary(BinaryData) ->
    <<MessageLength:1/big-unsigned-integer-unit:16, Rest1/binary>> = BinaryData,
    <<Message:MessageLength/bytes, Rest2/binary>> = Rest1,
    MessageString = erlang:binary_to_list(Message),
    [ _, ModuleName | _Command ] = string:tokens( MessageString, "_"),    
    File = erlang:list_to_existing_atom(ModuleName ++ "_pb"),
    Module = erlang:list_to_existing_atom(ModuleName),
    MessageRecord = erlang:list_to_existing_atom(string:to_lower(MessageString)),
    {Module, File:decode(MessageRecord, Rest2)}.


