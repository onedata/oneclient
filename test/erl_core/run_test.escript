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

%% Restart cluster before each test suite (+20 secs). 
-define(RESTART_CLUSTER, true).

-record(veil_document, {uuid = "", rev_info = 0, record = none, force_update = false}).
-define(CCM, central_cluster_manager).
-define(Node_Manager_Name, node_manager).
-define(Dispatcher_Name, request_dispatcher).
-define(Modules_With_Args, [{central_logger, []}, {cluster_rengine, []}, {control_panel, []}, {dao, []}, {fslogic, []}, {gateway, []}, {rtransfer, []}, {rule_manager, []}, {dns_worker, []}, {remote_files_manager, []}]).


main(["__exec" | [ TestName | Args ]]) ->
    set_up_net_kernel(),

    #veil_document{}, %todo delete

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

            io:format("Restarting nodes: ~p~n", [os:cmd("restart_cluster.sh " ++ os:getenv("CLUSTER_NODE"))]),
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

%%     CTX_CCM = call(?CCM_NODE_NAME, fun() -> setup(ccm, TestName) end),
%%     CTX_W = call(?WORKER_NODE_NAME, fun() -> setup(worker, TestName) end),

    CTX_CCM = rpc:call(?CCM_NODE_NAME, test_common,setup,[ccm, TestName]),
    CTX_W = rpc:call(?WORKER_NODE_NAME, test_common,setup,[worker, TestName]),

    CMD = "TEST_NAME=\"" ++ TestName ++"\" TEST_RUNNER=\"" ++ escript:script_name() ++ "\" ./" ++ TestName ++ "_i " ++ string:join(Args, " "),
    ?INFO("CMD: ~p", [CMD]),
    ?INFO("STDOUT: ~s", [os:cmd(CMD)]),
    
    rpc:call(?CCM_NODE_NAME, test_common, teardown, [ccm, TestName, CTX_CCM]),
    rpc:call(?WORKER_NODE_NAME, test_common, teardown, [worker, TestName, CTX_W]).


%%
%% Main SETPU/TEARDOWN methods
%%

%% 
%% HELPER METHODS
%%

%% call(Node, Module, Method, Args) ->
%%     Self = self(),
%%     pong = net_adm:ping(Node),
%%     io:format("~nTEST1: ~p~n",[rpc:call('worker@172.16.67.170',dao_lib,apply,[dao_users,list_users,[10,0],1])]),
%% %%     Pid = spawn(Node, fun() -> Self ! {self(), Fun()} end),
%%     rpc:call(Node,Mo)
%%     io:format("~nTEST2: ~p~n",[Pid]),
%%     receive
%%         {Pid, Ans} ->
%%             Ans
%%     after 150000 ->
%%         {error, timeout}
%%     end.

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



%% wait_for_cluster_init/0
%% ====================================================================
%% @doc Wait until cluster is initialized properly.
%% @end
-spec wait_for_cluster_init() -> Ans when
  Ans :: boolean() | {exception, E1, E2},
  E1 :: term(),
  E2 :: term().
%% ====================================================================
wait_for_cluster_init() ->
  wait_for_cluster_init(0).

%% wait_for_cluster_init/1
%% ====================================================================
%% @doc Wait until cluster is initialized properly.
%% @end
-spec wait_for_cluster_init(ModulesNum :: integer()) -> Ans when
  Ans :: boolean() | {exception, E1, E2},
  E1 :: term(),
  E2 :: term().
%% ====================================================================
wait_for_cluster_init(ModulesNum) ->
  wait_for_cluster_init(ModulesNum + length(?Modules_With_Args), 20).

%% wait_for_cluster_init/2
%% ====================================================================
%% @doc Wait until cluster is initialized properly.
%% @end
-spec wait_for_cluster_init(ModulesNum :: integer(), TriesNum :: integer()) -> Ans when
  Ans :: boolean() | {exception, E1, E2},
  E1 :: term(),
  E2 :: term().
%% ====================================================================
wait_for_cluster_init(ModulesNum, 0) ->
  check_init(ModulesNum);

wait_for_cluster_init(ModulesNum, TriesNum) ->
  case check_init(ModulesNum) of
    true -> true;
    _ ->
      timer:sleep(5000),
      wait_for_cluster_init(ModulesNum, TriesNum - 1)
  end.


%% check_init/1
%% ====================================================================
%% @doc Check if cluster is initialized properly.
%% @end
-spec check_init(ModulesNum :: integer()) -> Ans when
  Ans :: boolean() | {exception, E1, E2},
  E1 :: term(),
  E2 :: term().
%% ====================================================================
check_init(ModulesNum) ->
  try
    {WList, StateNum} = gen_server:call({global, ?CCM}, get_workers, 1000),
    case length(WList) >= ModulesNum of
      true ->
        timer:sleep(500),
        Nodes = gen_server:call({global, ?CCM}, get_nodes, 1000),
        {_, CStateNum} = gen_server:call({global, ?CCM}, get_callbacks, 1000),
        CheckNode = fun(Node, TmpAns) ->
          StateNum2 = gen_server:call({?Dispatcher_Name, Node}, get_state_num, 1000),
          {_, CStateNum2} = gen_server:call({?Dispatcher_Name, Node}, get_callbacks, 1000),
          case (StateNum == StateNum2) and (CStateNum == CStateNum2) of
            true -> TmpAns;
            false -> false
          end
        end,
        lists:foldl(CheckNode, true, Nodes);
      false ->
        false
    end
  catch
    E1:E2 ->
      {exception, E1, E2}
  end.


