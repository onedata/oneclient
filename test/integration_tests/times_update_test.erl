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
    DirectIORoot = filename:join([os:getenv(?TEST_ROOT_VAR), "directIO_root"]),
    os:putenv("DIO_ROOT", DirectIORoot), 
    fslogic_storage:insert_storage("DirectIO", [DirectIORoot]),
    DirectIORoot.


teardown(ccm, _State) ->
    ok; 
teardown(worker, _State) ->
    ok.


exec({env, VarName}) ->
    os:getenv(VarName).