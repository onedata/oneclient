%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements functions to contact the appmock server
%%% without the knowledge of underlying communication protocol. They are used
%%% for remote control of appmock instances during tests.
%%% @end
%%%-------------------------------------------------------------------
-module(appmock_client).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
-include("appmock_internal.hrl").

%% API
-export([rest_endpoint_request_count/3, verify_rest_history/2]).
-export([tcp_server_message_count/3, tcp_server_send/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Performs a request to an appmock instance to verify if a given endpoint mock
%% has been requested certain amount of times. Returns:
%% true - when verification succeded
%% {false, ActualNumber} - when verification failed; ActualNumber informs
%%     how many times has the endpoint been requested
%% {error, term()} - when there has been an error in verification
%% procedure (this implies a bug in appmock).
%% @end
%%--------------------------------------------------------------------
-spec rest_endpoint_request_count(Hostname :: binary(), Port :: integer(), Path :: binary()) ->
    true | {false, integer()} | {error, term()}.
rest_endpoint_request_count(Hostname, Port, Path) ->
    try
        JSON = appmock_utils:encode_to_json(?REST_ENDPOINT_REQUEST_COUNT_REQUEST(Port, Path)),
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            <<?REST_ENDPOINT_REQUEST_COUNT_PATH>>, post, [], JSON),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        ?REST_ENDPOINT_REQUEST_COUNT_UNPACK_RESPONSE(RespBody)
    catch T:M ->
        ?error("Error in verify_rest_endpoint - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs a request to an appmock instance to verify if the mocked endpoints
%% had been requested in correct order. ExpectedOrder is a list of {Port, Path} pairs
%% that define the expected order of requests. Returns:
%% true - when verification succeded
%% {false, ActualOrder} - when verification failed;
%%    ActualOrder is a list holding the requests in actual order.
%% {error, term()} - when there has been an error in verification
%% procedure (this implies a bug in appmock).
%% @end
%%--------------------------------------------------------------------
-spec verify_rest_history(Hostname :: binary(), ExpectedOrder :: PortPathMap) ->
    true | {false, PortPathMap} | {error, term()} when PortPathMap :: [{Port :: integer(), Path :: binary()}].
verify_rest_history(Hostname, ExpectedOrder) ->
    try
        JSON = appmock_utils:encode_to_json(?VERIFY_REST_HISTORY_PACK_REQUEST(ExpectedOrder)),
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            <<?VERIFY_REST_HISTORY_PATH>>, post, [], JSON),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        case RespBody of
            ?TRUE_RESULT ->
                true;
            _ ->
                History = ?VERIFY_REST_HISTORY_UNPACK_ERROR(RespBody),
                {false, History}
        end
    catch T:M ->
        ?error("Error in verify_rest_history - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs a request to an appmock instance to verify if the mocked endpoints
%% had been requested in correct order. ExpectedOrder is a list of {Port, Path} pairs
%% that define the expected order of requests. Returns:
%% true - when verification succeded
%% false - when verification failed
%% {error, term()} - when there has been an error in verification
%% procedure (this implies a bug in appmock).
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_message_count(Hostname :: binary(), Port :: integer(), Data :: binary()) ->
    true | false | {error, term()}.
tcp_server_message_count(Hostname, Port, Data) ->
    try
        Binary = ?TCP_SERVER_MESSAGE_COUNT_PACK_REQUEST(Data),
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        Path = ?TCP_SERVER_MESSAGE_COUNT_PATH(Port),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            <<(list_to_binary(Path))/binary>>, post, [], Binary),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        ?TCP_SERVER_MESSAGE_COUNT_UNPACK_RESPONSE(RespBody)
    catch T:M ->
        ?error("Error in tcp_server_message_count - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs a request to an appmock instance to verify if the mocked endpoints
%% had been requested in correct order. ExpectedOrder is a list of {Port, Path} pairs
%% that define the expected order of requests. Returns:
%% true - when verification succeded
%% false - when verification failed
%% {error, term()} - when there has been an error in verification
%% procedure (this implies a bug in appmock).
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_send(Hostname :: binary(), Port :: integer(), Data :: binary()) ->
    true | {error, term()}.
tcp_server_send(Hostname, Port, Data) ->
    try
        Binary = ?TCP_SERVER_SEND_PACK_REQUEST(Data),
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        Path = ?TCP_SERVER_SEND_PATH(Port),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            <<(list_to_binary(Path))/binary>>, post, [], Binary),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        case RespBody of
            ?TRUE_RESULT ->
                true;
            _ ->
                ?TCP_SERVER_SEND_UNPACK_ERROR(RespBody)
        end
    catch T:M ->
        ?error("Error in tcp_server_send - ~p:~p", [T, M]),
        {error, M}
    end.