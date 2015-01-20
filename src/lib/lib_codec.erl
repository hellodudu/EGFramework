-module(lib_codec).

-export([decode/1, encode/1]).

encode(Record) when erlang:is_tuple(Record) ->
    RecordNameAtom = erlang:element(1,Record),
    MessageName = erlang:atom_to_list(RecordNameAtom),
    [ _, ModuleName | _Command ] = string:tokens(MessageName, "_"),
    Encoder = erlang:list_to_existing_atom(ModuleName ++ "_pb"),
    EncodeFunction = erlang:list_to_existing_atom("encode_"++MessageName),
    SerializedDataBinary = erlang:iolist_to_binary(Encoder:EncodeFunction(Record)),
    ProtocolNum = proto_mapper:get(RecordNameAtom),
    RepliedIOData = << ProtocolNum:1/big-unsigned-integer-unit:32,SerializedDataBinary/binary >>,
    RepliedIOData.

decode(BinaryData) when erlang:is_binary(BinaryData) ->
    <<ProtocolNum:1/big-unsigned-integer-unit:32, Rest/binary>> = BinaryData,
    MessageAtom = proto_mapper:get(ProtocolNum),
    MessageName = erlang:atom_to_list(MessageAtom), 
    [ _, ModuleName | _Command ] = string:tokens(MessageName, "_"),
    Module = erlang:list_to_existing_atom(ModuleName),
    Decoder = erlang:list_to_existing_atom(ModuleName++"_pb"),
    {Module, Decoder:decode(MessageAtom, Rest)}.