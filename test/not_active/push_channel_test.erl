%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: PushChannelTest cluster side test driver.      
%% @end
%% ===================================================================
-module(push_channel_test).
-include("test_common.hrl").

-export([setup/1, teardown/2, exec/1]).


setup(ccm) ->
    ok;
setup(worker) ->
    DirectIORoot = "/tmp/dio",
    os:putenv("DIO_ROOT", DirectIORoot), 
    Fuse_groups = [{fuse_group_info, "cluster_fid", {storage_helper_info, "DirectIO", [DirectIORoot]}}],
    fslogic_storage:insert_storage("ClusterProxy", [], Fuse_groups),

    test_common:register_user("peer.pem"),
    ok.


teardown(ccm, _State) ->
    ok; 
teardown(worker, _State) ->
    ok.


exec({env, VarName}) ->
    os:getenv(VarName);

%% Send Msg to FUSE with FuseID
exec({push_msg, Msg, FuseID}) ->
    request_dispatcher:send_to_fuse(FuseID, {testchannelanswer, "test"}, "fuse_messages");
    
%% Get PUSH channel count for given FuseID
exec({get_handler_count, FuseID}) ->
    erlang:length(ets:lookup(dispatcher_callbacks_table, FuseID)).
