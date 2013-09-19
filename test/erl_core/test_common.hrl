%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header provides commonly used definitions in all integration tests 
%%       (mainly in clusters' setup/teardown methods)         
%% @end
%% ===================================================================
-ifndef(TEST_COMMON_HRL).
-define(TEST_COMMON_HRL, 1).

-define(COMMON_FILES_ROOT_VAR, "COMMON_FILES_ROOT").
-define(TEST_ROOT_VAR, "TEST_ROOT").
-define(COMMON_FILE(X), os:getenv(?COMMON_FILES_ROOT_VAR) ++ "/" ++ X).

-endif. %% TEST_COMMON_HRL