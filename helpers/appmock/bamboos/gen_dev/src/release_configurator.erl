%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides functions that configure erlang release
%%% by replacing envs in vm.args and sys.config
%%% @end
%%%-------------------------------------------------------------------
-module(release_configurator).
-author("Tomasz Lichon").

% oneprovider specific config
-define(ONEPROVIDER_CCM_APP_NAME, cluster_manager).
-define(DIST_APP_FAILOVER_TIMEOUT, timer:seconds(5)).
-define(SYNC_NODES_TIMEOUT, timer:minutes(1)).

%% API
-export([configure_release/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Configure release stored at ReleaseRootPath, according to given parameters
%% @end
%%--------------------------------------------------------------------
-spec configure_release(ApplicationName :: atom(), ReleaseRootPath :: string() | default,
    SysConfig :: list(), VmArgs :: list()) -> ok | no_return().
configure_release(?ONEPROVIDER_CCM_APP_NAME, ReleaseRootPath, SysConfig, VmArgs) ->
    {SysConfigPath, VmArgsPath} = find_config_location(?ONEPROVIDER_CCM_APP_NAME, ReleaseRootPath),
    lists:foreach(
        fun({Key, Value}) -> replace_vm_arg(VmArgsPath, "-" ++ atom_to_list(Key), Value) end,
        VmArgs
    ),

    lists:foreach(fun({AppName, AppConfig}) ->
        lists:foreach(fun({Key, Value}) ->
            replace_env(SysConfigPath, AppName, Key, Value)
        end, AppConfig)
    end, SysConfig),

    % configure kernel distributed erlang app
    NodeName = proplists:get_value(name, VmArgs),
    CmConfig = proplists:get_value(?ONEPROVIDER_CCM_APP_NAME, SysConfig),
    CmNodes = proplists:get_value(cm_nodes, CmConfig),
    case length(CmNodes) > 1 of
        true ->
            OptCms = CmNodes -- [list_to_atom(NodeName)],
            replace_application_config(SysConfigPath, kernel,
                [
                    {distributed, [{
                        ?ONEPROVIDER_CCM_APP_NAME,
                        ?DIST_APP_FAILOVER_TIMEOUT,
                        [list_to_atom(NodeName), list_to_tuple(OptCms)]
                    }]},
                    {sync_nodes_mandatory, OptCms},
                    {sync_nodes_timeout, ?SYNC_NODES_TIMEOUT}
                ]);
        false -> ok
    end;
configure_release(ApplicationName, ReleaseRootPath, SysConfig, VmArgs) ->
    {SysConfigPath, VmArgsPath} = find_config_location(ApplicationName, ReleaseRootPath),
    lists:foreach(
        fun({Key, Value}) -> replace_vm_arg(VmArgsPath, "-" ++ atom_to_list(Key), Value) end,
        VmArgs
    ),
    lists:foreach(fun({AppName, AppConfig}) ->
        lists:foreach(fun({Key, Value}) ->
            replace_env(SysConfigPath, AppName, Key, Value)
        end, AppConfig)
    end, SysConfig).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Reads erlang 'RELEASES' file in order to find where vm.args and
%% sys.config are located
%% @end
%%--------------------------------------------------------------------
-spec find_config_location(ApplicationName :: atom(), ReleaseRootPath :: string() | default) ->
    {SysConfigPath :: string(), VmArgsPath :: string()}.
find_config_location(ApplicationName, default) ->
    SysConfigPath = filename:join(["/etc", ApplicationName, "app.config"]),
    VmArgsPath = filename:join(["/etc", ApplicationName, "vm.args"]),
    {SysConfigPath, VmArgsPath};
find_config_location(ApplicationName, ReleaseRootPath) ->
    EtcDir = filename:join(ReleaseRootPath, "etc"),
    case filelib:is_dir(EtcDir) of
        true ->
            {filename:join(EtcDir, "app.config"), filename:join(EtcDir, "vm.args")};
        false ->
            ApplicationNameString = atom_to_list(ApplicationName),
            {ok, [[{release, ApplicationNameString, AppVsn, _, _, _}]]} =
                file:consult(filename:join([ReleaseRootPath, "releases", "RELEASES"])),
            SysConfigPath = filename:join([ReleaseRootPath, "releases", AppVsn, "sys.config"]),
            VmArgsPath = filename:join([ReleaseRootPath, "releases", AppVsn, "vm.args"]),
            {SysConfigPath, VmArgsPath}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Replace env in sys.config file
%% @end
%%--------------------------------------------------------------------
-spec replace_env(string(), atom(), atom(), term()) -> ok | no_return().
replace_env(SysConfigPath, ApplicationName, EnvName, EnvValue) ->
    {ok, [SysConfig]} = file:consult(SysConfigPath),
    ApplicationEnvs = proplists:get_value(ApplicationName, SysConfig),
    UpdatedApplicationEnvs = [{EnvName, EnvValue} | proplists:delete(EnvName, ApplicationEnvs)],
    replace_application_config(SysConfigPath, ApplicationName, UpdatedApplicationEnvs).

%%--------------------------------------------------------------------
%% @doc
%% Replace whole application config in sys.config file
%% @end
%%--------------------------------------------------------------------
-spec replace_application_config(string(), atom(), list()) -> ok | no_return().
replace_application_config(SysConfigPath, ApplicationName, ApplicationEnvs) ->
    {ok, [SysConfig]} = file:consult(SysConfigPath),
    UpdatedSysConfig =
        [{ApplicationName, ApplicationEnvs} | proplists:delete(ApplicationName, SysConfig)],
    ok = file:write_file(SysConfigPath, [term_to_string(UpdatedSysConfig), $.]).

%%--------------------------------------------------------------------
%% @doc
%% Replace env in vm.args file
%% @end
%%--------------------------------------------------------------------
-spec replace_vm_arg(string(), string(), string()) -> ok | no_return().
replace_vm_arg(VMArgsPath, FullArgName, ArgValue) ->
    {ok, Data} = file:read_file(VMArgsPath),
    NewData = re:replace(
        Data,
        <<(list_to_binary(FullArgName))/binary, " .*">>,
        <<(list_to_binary(FullArgName))/binary, " ", (list_to_binary(ArgValue))/binary>>
    ),
    ok = file:write_file(VMArgsPath, NewData).

%%--------------------------------------------------------------------
%% @doc
%% Convert erlang term to string
%% @end
%%--------------------------------------------------------------------
-spec term_to_string(term()) -> string().
term_to_string(Term) ->
    io_lib:fwrite("~p", [Term]).
