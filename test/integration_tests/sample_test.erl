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
    DirectIORoot = filename:join([os:getenv(?TEST_ROOT_VAR), "directIO_root"]),
    os:putenv("DIO_ROOT", DirectIORoot), 
    fslogic_storage:insert_storage("DirectIO", [DirectIORoot]),
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