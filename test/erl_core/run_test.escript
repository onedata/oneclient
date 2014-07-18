#!/usr/bin/env escript
%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This is an integration test runner. It runs GTEST test suite
%%       while managing and running setpu/teardown on cluster worker node environment.
%%       The script is used directly in 'make integration_tests' target.
%% @end
%% ===================================================================

-module(run_test).

-include("test_common.hrl").

-define(default_cookie, veil_cluster_node).
-define(default_ccm_name, "ccm").
-define(default_worker_name, "worker").

-define(CCM_NODE_NAME, list_to_atom(?default_ccm_name ++ "@" ++ os:getenv("CLUSTER_NODE"))).
-define(WORKER_NODE_NAME, list_to_atom(?default_worker_name ++ "@" ++ os:getenv("CLUSTER_NODE"))).

%% Restart cluster before each test suite (+20 secs). 
-define(RESTART_CLUSTER, true).

-define(Node_Manager_Name, node_manager).

main(["__exec" | [ TestName | Args ]]) ->
    set_up_net_kernel(),

    load_mods([?CCM_NODE_NAME, ?WORKER_NODE_NAME], [list_to_atom(TestName), test_common]),

    [Arg | _] = Args,
    {ok, Tokens,_EndLine} = erl_scan:string(Arg ++ "."),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),

    try rpc:call(?WORKER_NODE_NAME, list_to_atom(TestName), exec, [Value]) of
        Res ->
            IsString = io_lib:printable_unicode_list(Res),
            if 
                IsString -> io:format("~s", [Res]);
                true -> io:format("~p", [Res])
            end
    catch 
        Type:Error -> io:format("[~p] ~p", [Type, Error])
    end;    
main([TestName | Args]) -> 
    set_up_net_kernel(),

    if
        ?RESTART_CLUSTER ->
            os:cmd("curl -X DELETE " ++ os:getenv("CLUSTER_NODE") ++ ":5984/files"),
            os:cmd("curl -X DELETE " ++ os:getenv("CLUSTER_NODE") ++ ":5984/system_data"),
            os:cmd("curl -X DELETE " ++ os:getenv("CLUSTER_NODE") ++ ":5984/file_descriptors"),
            os:cmd("curl -X DELETE " ++ os:getenv("CLUSTER_NODE") ++ ":5984/users"),

            DirectIORoot = "/tmp/dio",
            io:format("Delete DirectIO dir: ~p~n", [os:cmd("ssh root@" ++ os:getenv("CLUSTER_NODE") ++ " rm -rf " ++ DirectIORoot)]),
            io:format("Create DirectIO dir: ~p~n", [os:cmd("ssh root@" ++ os:getenv("CLUSTER_NODE") ++ " mkdir -p " ++ DirectIORoot)]),

            io:format("Restarting nodes: ~p~n", [os:cmd("ssh root@" ++ os:getenv("CLUSTER_NODE") ++ " /etc/init.d/veil restart")]),
            timer:sleep(10000), %% Give node some time to boot 

            pong = net_adm:ping(?CCM_NODE_NAME),
            pong = net_adm:ping(?WORKER_NODE_NAME),
            gen_server:cast({?Node_Manager_Name, ?WORKER_NODE_NAME}, do_heart_beat),
            timer:sleep(1000),
            gen_server:cast({global, ?CCM}, init_cluster),

            timer:sleep(10000); %% Give cluster some time to boot
             
        true -> ok
    end,

    env_setup([?CCM_NODE_NAME, ?WORKER_NODE_NAME]),
    load_mods([?CCM_NODE_NAME, ?WORKER_NODE_NAME], [list_to_atom(TestName), test_common]),

    CTX_CCM = rpc:call(?CCM_NODE_NAME, test_common,setup,[ccm, TestName]),
    CTX_W = rpc:call(?WORKER_NODE_NAME, test_common,setup,[worker, TestName]),

    CMD = "TEST_NAME=\"" ++ TestName ++"\" TEST_RUNNER=\"" ++ escript:script_name() ++ "\" ./" ++ TestName ++ "_i " ++ string:join(Args, " "),
    ?INFO("CMD: ~p", [CMD]),
    ?INFO("STDOUT: ~s", [os:cmd(CMD)]),
    
    rpc:call(?CCM_NODE_NAME, test_common, teardown, [ccm, TestName, CTX_CCM]),
    rpc:call(?WORKER_NODE_NAME, test_common, teardown, [worker, TestName, CTX_W]).

%% 
%% HELPER METHODS
%%

set_up_net_kernel() ->
    {A, B, C} = erlang:now(),
    NodeName = "setup_node_" ++ integer_to_list(A, 32) ++ integer_to_list(B, 32) ++ integer_to_list(C, 32) ++ "@127.0.0.1",
    net_kernel:start([list_to_atom(NodeName), longnames]),
    erlang:set_cookie(node(), ?default_cookie).

load_mods(_Nodes, []) ->
    ok;
load_mods(Nodes, [Module | Rest]) ->
    {Mod, Bin, File} = code:get_object_code(Module),
    {_, _} = rpc:multicall(Nodes, code, delete, [Mod]),
    {_, _} = rpc:multicall(Nodes, code, purge, [Mod]),
    {_Replies, _} = rpc:multicall(Nodes, code, load_binary, [Mod, File, Bin]),
    load_mods(Nodes, Rest).


env_setup(Nodes) ->
    rpc:multicall(Nodes, os, putenv, [?TEST_ROOT_VAR, os:getenv(?TEST_ROOT_VAR)]),
    rpc:multicall(Nodes, os, putenv, [?COMMON_FILES_ROOT_VAR, os:getenv(?COMMON_FILES_ROOT_VAR)]).
