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

-define(INFO(X, Y), io:format(X ++ "~n", Y)).

-define(default_cookie, veil_cluster_node).
-define(default_ccm_name, "ccm").
-define(default_worker_name, "worker").

-define(CCM_NODE_NAME, list_to_atom(?default_ccm_name ++ "@" ++ os:getenv("CLUSTER_NODE"))).
-define(WORKER_NODE_NAME, list_to_atom(?default_worker_name ++ "@" ++ os:getenv("CLUSTER_NODE"))).

-define(CCM, central_cluster_manager).
-define(Node_Manager_Name, node_manager).

%% Restart cluster before each test suite (+20 secs). 
-define(RESTART_CLUSTER, true).


main(["__exec" | [ TestName | Args ]]) ->
    set_up_net_kernel(),

    load_mods([?CCM_NODE_NAME, ?WORKER_NODE_NAME], [list_to_atom(TestName), test_common]),

    [Arg | _] = Args,
    {ok, Tokens,_EndLine} = erl_scan:string(Arg ++ "."),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),

    try call(?WORKER_NODE_NAME, fun() -> apply(list_to_atom(TestName), exec, [Value]) end) of 
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

            os:cmd("restart_cluster.sh " ++ os:getenv("CLUSTER_NODE")),
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

    CTX_CCM = call(?CCM_NODE_NAME, fun() -> setup(ccm, TestName) end),
    CTX_W = call(?WORKER_NODE_NAME, fun() -> setup(worker, TestName) end),
    
    CMD = "TEST_NAME=\"" ++ TestName ++"\" TEST_RUNNER=\"" ++ escript:script_name() ++ "\" ./" ++ TestName ++ "_i " ++ string:join(Args, " "),
    ?INFO("CMD: ~p", [CMD]),
    ?INFO("STDOUT: ~s", [os:cmd(CMD)]),
    
    call(?CCM_NODE_NAME, fun() -> teardown(ccm, TestName, CTX_CCM) end),
    call(?WORKER_NODE_NAME, fun() -> teardown(worker, TestName, CTX_W) end).


%%
%% Main SETPU/TEARDOWN methods
%%

%% Setup runs on cluster node !
setup(worker, TestName) ->
    
    WipeRes = 
        try test_common:wipe_db(all) of 
            Res -> Res
        catch
            Type:Error -> {Type, Error, erlang:get_stacktrace()}
        end,
    RegisterRes = 
        try test_common:register_user("peer.pem") of 
            Res1 -> Res1
        catch
            Type1:Error1 -> {Type1, Error1, erlang:get_stacktrace()}
        end,

    ?INFO("DB Wipe result: ~p", [WipeRes]),
    ?INFO("Register test user result: ~p", [RegisterRes]),

    setup1(worker, TestName);
setup(NodeType, TestName) ->
    setup1(NodeType, TestName).

setup1(NodeType, TestName) ->
    %% Run test specific setup method
    R = 
    try apply(list_to_atom(TestName), setup, [NodeType]) of
        Res2 -> Res2 
    catch 
        Type2:Error2 -> {Type2, Error2, erlang:get_stacktrace()}
    end,
    ?INFO("Setup {~p, ~p}: ~p", [NodeType, TestName, R]).
   

%% Teardown runs on cluster node !    
teardown(NodeType, TestName, CTX) ->
    ?INFO("TearDown: ~p:~p (CTX: ~p)", [NodeType, TestName, CTX]),

    %% Run test specific teardown method
    try apply(list_to_atom(TestName), teardown, [NodeType, CTX]) of 
        Res1 -> Res1 
    catch 
        Type1:Error1 -> {Type1, Error1, erlang:get_stacktrace()}
    end.


%% 
%% HELPER METHODS
%%

call(Node, Fun) ->
    Self = self(),
    pong = net_adm:ping(Node), 
    Pid = spawn(Node, fun() -> Self ! {self(), Fun()} end),
    receive 
        {Pid, Ans} ->
            Ans
    after 10000 ->
        {error, timeout}
    end.

set_up_net_kernel() ->
    {A, B, C} = erlang:now(),
    NodeName = "setup_node_" ++ integer_to_list(A, 32) ++ integer_to_list(B, 32) ++ integer_to_list(C, 32) ++ "@127.0.0.1",
    net_kernel:start([list_to_atom(NodeName), longnames]),
    erlang:set_cookie(node(), ?default_cookie).

load_mods(_Nodes, []) ->
    ok;
load_mods(Nodes, [Module | Rest]) ->
    {Mod, Bin, File} = code:get_object_code(Module),
    {_, _} = rpc:multicall(Nodes, code, purge, [Mod]),
    {_Replies, _} = rpc:multicall(Nodes, code, load_binary, [Mod, Bin]),
    load_mods(Nodes, Rest).


env_setup(Nodes) ->
    rpc:multicall(Nodes, os, putenv, [?TEST_ROOT_VAR, os:getenv(?TEST_ROOT_VAR)]),
    rpc:multicall(Nodes, os, putenv, [?COMMON_FILES_ROOT_VAR, os:getenv(?COMMON_FILES_ROOT_VAR)]).