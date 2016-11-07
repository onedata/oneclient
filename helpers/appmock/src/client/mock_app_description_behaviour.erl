%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour defines an API that has to be implemented by app description modules.
%%% App description module is an .erl file that contains list of endpoints and other configuration
%%% needed for appmock server to describe its behaviour. Such file is required to start an appmock instance,
%%% and will be compiled and loaded dynamically.
%%% @end
%%%-------------------------------------------------------------------
-module(mock_app_description_behaviour).
-author("Lukasz Opiola").

-include("appmock.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Callback called before the initialization of appmock instance. Must return a list of
%% HTTPS endpoint mocks. It is used to determine which ports and dispatch rules to use.
%% @end
%%--------------------------------------------------------------------
-callback rest_mocks() -> [#rest_mock{}].


%%--------------------------------------------------------------------
%% @doc
%% Callback called before the initialization of appmock instance. Must return a list of
%% TCP server mocks.
%% @end
%%--------------------------------------------------------------------
-callback tcp_server_mocks() -> [#tcp_server_mock{}].