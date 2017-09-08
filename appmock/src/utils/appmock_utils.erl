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

-include("appmock_internal.hrl").

%% API
-export([load_description_module/1]).
-export([rc_request/3, rc_request/4, rc_request/5]).

%%%===================================================================
%%% API
%%%===================================================================

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


%%--------------------------------------------------------------------
%% @doc
%% Performs a request on a remote control endpoint running on given Hostname.
%% @equiv remote_control_request(Method, Hostname, Path, [])
%% @end
%%--------------------------------------------------------------------
-spec rc_request(Method :: http_client:method(), Hstnm :: string() | binary(),
    Path :: string() | binary()) ->
    {ok, http_client:code(), http_client:headers(), http_client:body()} |
    {error, term()}.
rc_request(Method, Hostname, Path) ->
    rc_request(Method, Hostname, Path, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Performs a request on a remote control endpoint running on given Hostname.
%% @equiv remote_control_request(Method, Hostname, Path, Headers, <<>>)
%% @end
%%--------------------------------------------------------------------
-spec rc_request(Method :: http_client:method(), Hstnm :: string() | binary(),
    Path :: string() | binary(), Headers :: http_client:headers()) ->
    {ok, http_client:code(), http_client:headers(), http_client:body()} |
    {error, term()}.
rc_request(Method, Hostname, Path, Headers) ->
    rc_request(Method, Hostname, Path, Headers, <<>>).


%%--------------------------------------------------------------------
%% @doc
%% Performs a request on a remote control endpoint running on given Hostname.
%% @end
%%--------------------------------------------------------------------
-spec rc_request(Method :: http_client:method(), Hstnm :: string() | binary(),
    Path :: string() | binary(), Headers :: http_client:headers(),
    Body :: http_client:body()) ->
    {ok, http_client:code(), http_client:headers(), http_client:body()} |
    {error, term()}.
rc_request(Method, Hostname, Path, Headers, Body) ->
    {ok, RmteCntrlPort} = application:get_env(?APP_NAME, remote_control_port),
    URL = str_utils:format("https://~s:~B~s", [Hostname, RmteCntrlPort, Path]),
    % insecure option disables server cert verification
    http_client:request(Method, URL, Headers, Body, [insecure]).


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
