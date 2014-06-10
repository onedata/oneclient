%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: SampleTest cluster side test driver.      
%% @end
%% ===================================================================
-module(sample_test).
-include("test_common.hrl").

-export([setup/1, teardown/2, exec/1]).


%% This method will be called before each test suite (suite  = whole test translation unit, one .cc test file - one setup/1 call)
setup(ccm) ->
    ok;
setup(worker) ->
    DirectIORoot = "/tmp/dio",
    os:cmd("rm -rf " ++ DirectIORoot),
    os:cmd("mkdir -p " ++ DirectIORoot),
    os:putenv("DIO_ROOT", DirectIORoot), 
    Fuse_groups = [{fuse_group_info, "cluster_fid", {storage_helper_info, "DirectIO", [DirectIORoot]}}],
    fslogic_storage:insert_storage("ClusterProxy", [], Fuse_groups),
    
    test_common:register_user("peer.pem"),

    DirectIORoot.


%% Same as setup/1 except that teardown is called after test suite
%% State argument is an return value of setup/1
teardown(ccm, _State) ->
    ok; 
teardown(worker, _State) ->
    ok.


%% This method will be called on a cluster when you use erlExec(string) function in test suite
%% C++ erlEcec string argument will be translated to erlang term (e.g. "{1, ok}" will eval to {1, ok}). 
%% Also you have to return a string. All other terms will be translated to string using io:format 
exec({env, VarName}) ->
    os:getenv(VarName).