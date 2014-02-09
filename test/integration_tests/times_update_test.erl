%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: TimeUpdateTest cluster side test driver.      
%% @end
%% ===================================================================
-module(times_update_test).
-include("test_common.hrl").

-export([setup/1, teardown/2, exec/1]).


setup(ccm) ->
    ok;
setup(worker) ->
    DirectIORoot = "/tmp/dio",
    file:del_dir(DirectIORoot),
    file:make_dir(DirectIORoot),
    os:putenv("DIO_ROOT", DirectIORoot), 
    Fuse_groups = [{fuse_group_info, "cluster_fid", {storage_helper_info, "DirectIO", [DirectIORoot]}}],
    fslogic_storage:insert_storage("ClusterProxy", [], Fuse_groups),
    DirectIORoot.


teardown(ccm, _State) ->
    ok; 
teardown(worker, _State) ->
    ok.


exec({env, VarName}) ->
    os:getenv(VarName).