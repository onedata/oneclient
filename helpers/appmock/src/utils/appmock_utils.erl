%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains various utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(appmock_utils).
-author("Lukasz Opiola").

%% API
-export([https_request/6, encode_to_json/1, decode_from_json/1, load_description_module/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Performs a single request using ibrowse.
%% @end
%%--------------------------------------------------------------------
-spec https_request(HostnameBin :: binary(), Port :: integer(), PathBin :: binary(), Method :: atom(), Headers :: [], Body :: term()) ->
    {Code :: integer(), RespHeader :: [{binary(), binary()}], RespBody :: binary()}.
https_request(HostnameBin, Port, PathBin, Method, HeadersBin, Body) ->
    Hostname = gui_str:to_list(HostnameBin),
    Path = gui_str:to_list(PathBin),
    Headers = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- HeadersBin],
    {ok, Code, RespHeadersBin, RespBody} = ibrowse:send_req(
        "https://" ++ Hostname ++ ":" ++ integer_to_list(Port) ++ Path,
        Headers, Method, Body, [{response_format, binary}]),
    RespHeaders = [{list_to_binary(K), list_to_binary(V)} || {K, V} <- RespHeadersBin],
    {list_to_integer(Code), RespHeaders, RespBody}.


%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert an erlang term to JSON, producing
%% binary result. The output is in UTF8 encoding.
%% Possible terms, can be nested:
%% {struct, Props} - Props is a structure as a proplist, e.g.: [{id, 13}, {message, "mess"}]
%% {Props} - alias for above
%% {array, Array} - Array is a list, e.g.: [13, "mess"]
%% @end
%%--------------------------------------------------------------------
-spec encode_to_json(term()) -> binary().
encode_to_json(Term) ->
    Encoder = mochijson2:encoder([{utf8, true}]),
    iolist_to_binary(Encoder(Term)).


%%--------------------------------------------------------------------
%% @doc
%% Convenience function that convert JSON binary to an erlang term.
%% @end
%%--------------------------------------------------------------------
-spec decode_from_json(binary()) -> term().
decode_from_json(JSON) ->
    try mochijson2:decode(JSON, [{format, proplist}]) catch _:_ -> throw(invalid_json) end.


%%--------------------------------------------------------------------
%% @doc
%% Compiles and loads a given .erl file.
%% @end
%%--------------------------------------------------------------------
-spec load_description_module(FilePath :: string() | module()) -> module() | no_return().
load_description_module(FilePath) when is_atom(FilePath) ->
    load_description_module(atom_to_list(FilePath));
load_description_module(FilePath) ->
    try
        {ok, TmpFileCopyPath} = create_tmp_copy_with_erl_extension(FilePath),
        FileName = filename:basename(TmpFileCopyPath),
        {ok, ModuleName} = compile:file(TmpFileCopyPath),
        {ok, Bin} = file:read_file(filename:rootname(FileName) ++ ".beam"),
        erlang:load_module(ModuleName, Bin),
        cleanup_tmp_copy(FilePath, TmpFileCopyPath),
        ModuleName
    catch T:M ->
        cleanup_tmp_copy(FilePath, convert_from_cfg_to_erl_ext(FilePath)),
        throw({invalid_app_description_module, {type, T}, {message, M}, {stacktrace, erlang:get_stacktrace()}})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Convert:
%% "foo.erl.cfg" -> "foo.erl"
%% "bar.erl" -> "bar.erl"
%% @end
%%--------------------------------------------------------------------
-spec convert_from_cfg_to_erl_ext(string()) -> string().
convert_from_cfg_to_erl_ext(String) ->
    case lists:reverse(String) of
        [$g, $f, $c, $. | Name] -> lists:reverse(Name);
        _ -> String
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates temporary copy of file, ommiting optional ".cfg" extension,
%% so file may be compiled as erlang module
%% @end
%%--------------------------------------------------------------------
-spec create_tmp_copy_with_erl_extension(string()) -> {ok, Copy :: string()}.
create_tmp_copy_with_erl_extension(FilePath) ->
    FilePathWithProperExt = convert_from_cfg_to_erl_ext(FilePath),
    utils:cmd(["cp", "-f", FilePath, FilePathWithProperExt]),
    {ok, FilePathWithProperExt}.

%%--------------------------------------------------------------------
%% @doc
%% Removes TmpCopy, if its path is different from FilePath
%% @end
%%--------------------------------------------------------------------
-spec cleanup_tmp_copy(FilePath :: string(), TmpCopyPath :: string()) ->
    ok | {error, term()}.
cleanup_tmp_copy(FilePath, TmpCopyPath) when FilePath =:= TmpCopyPath ->
    ok;
cleanup_tmp_copy(_, TmpCopyPath) ->
    file:delete(TmpCopyPath).
