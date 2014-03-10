%% @author anthonyjiang
%% @doc @todo Add description to compile.


-module(dev).

%% ====================================================================
%% API functions
%% ====================================================================
-export([compile_all/0]).
-export([compile/0]).


%% ====================================================================
%% Internal functions
%% ====================================================================

compile_all() ->
    {ok, FileList} = file:list_dir("."),
    lists:foreach( fun(File)->
                           case filename:extension(File) of
                               ".proto" -> 
                                   protobuffs_compile:scan_file(File, [{output_ebin_dir, "../ebin"},{output_include_dir, "../include"}]);
                               _ ->
                                   ignore
                           end
                   end,
                   FileList),
    erlang:halt().

compile() ->
    {ok, FileList} = file:list_dir("."),
    lists:foreach(fun(SourceFile)->
        case filename:extension(SourceFile) of
            ".proto" ->
                [FileName, "proto"] = string:tokens(SourceFile, "."),
                TargetFile="../ebin/" ++ FileName ++ "_pb.beam",
                SourceLastModifiedDatetime = filelib:last_modified(SourceFile),
                TargetLastModifiedDatetime = filelib:last_modified(TargetFile),
                case SourceLastModifiedDatetime > TargetLastModifiedDatetime of
                    true ->
                        protobuffs_compile:scan_file(SourceFile, [{output_ebin_dir, "../ebin"}, {output_include_dir, "../include"}]);
                    _ ->
                        ignore
                end;
            _ ->
                ignore
        end
    end,
    FileList),
    io:format("~n"),
    erlang:halt().
        