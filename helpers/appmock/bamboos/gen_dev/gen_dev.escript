#!/usr/bin/env escript
%% -*- erlang -*-
%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% gen_dev script uses configurator.erl to configure release according to
%%% given gen_dev_args configuration. For each entry in gen_dev_args, the
%%% fresh release is copied from _build/default/rel/?APP_NAME to 'target_dir', and
%%% vm.args/sys.config are configured properly.
%%% @end
%%%-------------------------------------------------------------------
-module(gen_dev).

-define(ARGS_FILE, "gen_dev_args.json").
-define(EXIT_FAILURE_CODE, 1).

%% API
-export([main/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Script entry function.
%% @end
%%--------------------------------------------------------------------
-spec main(Args :: [string()]) -> no_return().
main([]) ->
    main([filename:join(get_escript_dir(), ?ARGS_FILE)]);
main([ArgsFile]) ->
    try
        helpers_init(),
        ParsedJson = args_parser:parse_config_file(ArgsFile),
        ok = configure_apps(ParsedJson)
    catch
        _Type:Error ->
            Stacktrace = erlang:get_stacktrace(),
            try logger:print("Error: ~ts", [Error])
            catch _:_ -> logger:print("Error: ~p", [Error])
            end,
            logger:print("Stacktrace: ~p~n", [Stacktrace]),
            halt(?EXIT_FAILURE_CODE)
    end;
main(_) ->
    logger:print_usage(),
    halt(?EXIT_FAILURE_CODE).

%%%===================================================================
%%% Application configuration
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Configure release nodes for multiple applications
%% @end
%%--------------------------------------------------------------------
-spec configure_apps([{AppName :: atom(), ClusterNodesDescription :: list()}]) -> ok.
configure_apps([]) ->
    ok;
configure_apps([{AppName, ClusterNodesDescription} | Rest]) ->
    Config = proplists:get_value(config, ClusterNodesDescription),
    Nodes = proplists:get_value(nodes, ClusterNodesDescription),
    ok = create_releases(AppName, Config, Nodes),
    configure_apps(Rest).

%%--------------------------------------------------------------------
%% @doc
%% Configure release nodes of given app.
%% 'Config' contains general gen_dev configuration, i. e. paths to release binaries.
%% 'NodeName' is the directory name of newly created node.
%% 'NodeConfig' contains environment variables for
%%      sys.config and vm.args of newly created node
%% @end
%%--------------------------------------------------------------------
-spec create_releases(AppName :: atom(), Config :: list(), [{NodeName :: atom(), NodeConfig :: list()}]) -> ok.
create_releases(_, _, []) ->
    ok;
create_releases(AppName, Config, [{Name, NodeConfig} | Rest]) ->
    {InputDir, TargetDir} = prepare_neccessary_paths(Config),
    ReleaseDir = prepare_fresh_release(InputDir, TargetDir, Name),
    {SysConfig, VmArgs} = prepare_and_print_configuration(AppName, InputDir, ReleaseDir, NodeConfig),
    release_configurator:configure_release(AppName, ReleaseDir, SysConfig, VmArgs),
    create_releases(AppName, Config, Rest).

%%--------------------------------------------------------------------
%% @doc
%% Get from config:
%% 'InputDir' - path to fresh app release
%% 'TargetDir' - path where new releases will be created, i. e.
%%      'TargetDir'/node1, 'TargetDir'/node2 etc.
%% @end
%%--------------------------------------------------------------------
-spec prepare_neccessary_paths(Config :: list()) -> {InputDir :: string(), TargetDir :: string()}.
prepare_neccessary_paths(Config) ->
    InputDir = proplists:get_value(input_dir, Config),
    TargetDir = proplists:get_value(target_dir, Config),
    filesystem_operations:make_dir(TargetDir),
    {InputDir, TargetDir}.

%%--------------------------------------------------------------------
%% @doc
%% Extracts sys.config and vm.args configuration from 'NodeConfig'.
%% Additionally, function pretty prints summary of release configuration.
%% @end
%%--------------------------------------------------------------------
-spec prepare_and_print_configuration(AppName :: atom(), InputDir :: string(),
    ReleaseDir :: string(), NodeConfig :: list()) -> {SysConfig :: list(), VmArgs :: list()}.
prepare_and_print_configuration(AppName, InputDir, ReleaseDir, NodeConfig) ->
    logger:print("================ Configuring release ===================="),
    logger:pretty_print_entry({application, AppName}),
    logger:pretty_print_entry({input_dir, InputDir}),
    logger:pretty_print_entry({release_dir, ReleaseDir}),

    logger:print("====================== vm.args =========================="),
    VmArgs = proplists:get_value('vm.args', NodeConfig),
    lists:foreach(fun(X) -> logger:pretty_print_entry(X) end, VmArgs),

    logger:print("===================== sys.config ========================"),
    SysConfig = proplists:get_value('sys.config', NodeConfig),
    lists:foreach(fun({ConfiguredApp, AppConfig}) ->
        logger:pretty_print_entry({environment, ConfiguredApp}),
        lists:foreach(fun(X) -> logger:pretty_print_entry(X, 1) end, AppConfig),
        logger:print("")
    end, SysConfig),
    logger:print(""),
    {SysConfig, VmArgs}.

%%--------------------------------------------------------------------
%% @doc
%% Copies fresh release from 'InputDir' to 'TargetDir'/'Name' later called ReleaseDir.
%% The ReleaseDir is returned.
%% @end
%%--------------------------------------------------------------------
-spec prepare_fresh_release(InputDir :: string(), TargetDir :: string(), Name :: atom()) -> ReleaseDir :: string().
prepare_fresh_release(InputDir, TargetDir, Name) ->
    ReleaseDir = filename:join(TargetDir, atom_to_list(Name)),
    filesystem_operations:copy_dir(InputDir, ReleaseDir),
    ReleaseDir.

%%%===================================================================
%%% Helper modules compilation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Loads helper modules.
%% @end
%%--------------------------------------------------------------------
-spec helpers_init() -> ok.
helpers_init() ->
    SrcDir = filename:join(get_escript_dir(), "src"),
    {ok, Modules} = file:list_dir(SrcDir),
    lists:foreach(fun(Module) ->
        compile_and_load_module(filename:join(SrcDir, Module))
    end, Modules).

%%--------------------------------------------------------------------
%% @doc
%% Compiles and loads module into erlang VM.
%% @end
%%--------------------------------------------------------------------
-spec compile_and_load_module(File :: string()) -> ok.
compile_and_load_module(File) ->
    {ok, ModuleName, Binary} = compile:file(File, [report, binary]),
    {module, _} = code:load_binary(ModuleName, File, Binary),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Get path of current escript dir
%% @end
%%--------------------------------------------------------------------
-spec get_escript_dir() -> string().
get_escript_dir() ->
    filename:dirname(escript:script_name()).
