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
-export([rest_endpoint_request_count/3, verify_rest_history/2, reset_rest_history/1]).

-export([tcp_server_specific_message_count/3, tcp_server_wait_for_specific_messages/7]).
-export([tcp_server_all_messages_count/2, tcp_server_wait_for_any_messages/6]).
-export([tcp_server_send/4, tcp_server_history/2, reset_tcp_server_history/1]).
-export([tcp_server_connection_count/2, tcp_server_wait_for_connections/5]).

% These defines determine how often the appmock server will be requested to check for condition
% when waiting for something. Increment rate causes each next interval to be longer
-define(WAIT_STARTING_CHECK_INTERVAL, 250).
-define(WAIT_INTERVAL_INCREMENT_RATE, 1.3).

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
%% Performs a request to an appmock instance to reset all the history connected with ALL mocked rest endpoints.
%% The reset will cause this instance to act the same as if it was restarted clean.
%% @end
%%--------------------------------------------------------------------
-spec reset_rest_history(Hostname :: binary()) -> true | {error, term()}.
reset_rest_history(Hostname) ->
    try
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            <<?RESET_REST_HISTORY_PATH>>, post, [], <<"">>),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        case RespBody of
            ?TRUE_RESULT ->
                true
        end
    catch T:M ->
        ?error("Error in reset_rest_history - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs a request to an appmock instance to check the number of
%% specific messages received by a TCP endpoint. Returns:
%% Count - number of requests
%% {error, term()} - when there has been an error.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_specific_message_count(Hostname :: binary(), Port :: integer(), Data :: binary()) ->
    integer() | {error, term()}.
tcp_server_specific_message_count(Hostname, Port, Data) ->
    try
        Binary = ?TCP_SERVER_SPECIFIC_MESSAGE_COUNT_PACK_REQUEST(Data),
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        Path = list_to_binary(?TCP_SERVER_SPECIFIC_MESSAGE_COUNT_PATH(Port)),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            Path, post, [], Binary),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        ?TCP_SERVER_SPECIFIC_MESSAGE_COUNT_UNPACK_RESPONSE(RespBody)
    catch T:M ->
        ?error("Error in tcp_server_specific_message_count - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns when given number of given messages have been sent on given port, or after it timeouts.
%% The AcceptMore flag makes the function succeed when there is the same or more messages than expected.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_wait_for_specific_messages(Hostname :: binary(), Port :: integer(), Data :: binary(),
    MessageCount :: binary(), AcceptMore :: boolean(), ReturnHistory :: boolean(), Timeout :: integer()) ->
    ok | {error, term()}.
tcp_server_wait_for_specific_messages(Hostname, Port, Data, MessageCount, AcceptMore, ReturnHistory, Timeout) ->
    try
        StartingTime = now(),
        CheckMessNum = fun(ThisFun, WaitFor) ->
            case tcp_server_specific_message_count(Hostname, Port, Data) of
                {ok, Result} when AcceptMore andalso Result >= MessageCount ->
                    ok;
                {ok, Result} when Result =:= MessageCount ->
                    ok;
                {ok, _} ->
                    case utils:milliseconds_diff(now(), StartingTime) > Timeout of
                        true ->
                            {error, timeout};
                        false ->
                            timer:sleep(WaitFor),
                            ThisFun(ThisFun, trunc(WaitFor * ?WAIT_INTERVAL_INCREMENT_RATE))
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        Res = CheckMessNum(CheckMessNum, ?WAIT_STARTING_CHECK_INTERVAL),
        case {Res, ReturnHistory} of
            {ok, false} ->
                ok;
            {ok, true} ->
                tcp_server_history(Hostname, Port);
            _ ->
                Res
        end
    catch T:M ->
        ?error("Error in tcp_server_wait_for_specific_messages - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs a request to an appmock instance to check the number of
%% all messages received by a TCP endpoint. Returns:
%% Count - number of requests
%% {error, term()} - when there has been an error.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_all_messages_count(Hostname :: binary(), Port :: integer()) ->
    integer() | {error, term()}.
tcp_server_all_messages_count(Hostname, Port) ->
    try
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        Path = list_to_binary(?TCP_SERVER_ALL_MESSAGES_COUNT_PATH(Port)),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            Path, post, [], <<"">>),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        ?TCP_SERVER_ALL_MESSAGES_COUNT_UNPACK_RESPONSE(RespBody)
    catch T:M ->
        ?error("Error in tcp_server_all_messages_count - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns when given number of given messages have been sent on given port, or after it timeouts.
%% The AcceptMore flag makes the function succeed when there is the same or more messages than expected.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_wait_for_any_messages(Hostname :: binary(), Port :: integer(), MessageCount :: binary(),
    AcceptMore :: boolean(), ReturnHistory :: boolean(), Timeout :: integer()) -> ok | {ok, [binary()]} | {error, term()}.
tcp_server_wait_for_any_messages(Hostname, Port, MessageCount, AcceptMore, ReturnHistory, Timeout) ->
    try
        StartingTime = now(),
        CheckMessNum = fun(ThisFun) ->
            case tcp_server_all_messages_count(Hostname, Port) of
                {ok, Result} when AcceptMore andalso Result >= MessageCount ->
                    ok;
                {ok, Result} when Result =:= MessageCount ->
                    ok;
                {ok, _} ->
                    case utils:milliseconds_diff(now(), StartingTime) > Timeout of
                        true ->
                            {error, timeout};
                        false ->
                            timer:sleep(?WAIT_STARTING_CHECK_INTERVAL),
                            ThisFun(ThisFun)
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        Res = CheckMessNum(CheckMessNum),
        case {Res, ReturnHistory} of
            {ok, false} ->
                ok;
            {ok, true} ->
                tcp_server_history(Hostname, Port);
            _ ->
                Res
        end
    catch T:M ->
        ?error("Error in tcp_server_wait_for_any_messages - ~p:~p", [T, M]),
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
-spec tcp_server_send(Hostname :: binary(), Port :: integer(), Data :: binary(), MessageCount :: integer()) ->
    true | {error, term()}.
tcp_server_send(Hostname, Port, Data, MessageCount) ->
    try
        Binary = ?TCP_SERVER_SEND_PACK_REQUEST(Data),
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        Path = list_to_binary(?TCP_SERVER_SEND_PATH(Port, MessageCount)),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            Path, post, [], Binary),
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


%%--------------------------------------------------------------------
%% @doc
%% Performs a request to an appmock instance to reset all the history connected with ALL mocked TCP endpoints.
%% The reset will cause this instance to act the same as if it was restarted clean - e. g. counters will be reset.
%% Existing connections WILL NOT BE DISTURBED.
%% @end
%%--------------------------------------------------------------------
-spec reset_tcp_server_history(Hostname :: binary()) -> true | {error, term()}.
reset_tcp_server_history(Hostname) ->
    try
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            <<?RESET_TCP_SERVER_HISTORY_PATH>>, post, [], <<"">>),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        case RespBody of
            ?TRUE_RESULT ->
                true
        end
    catch T:M ->
        ?error("Error in reset_tcp_history - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs a request to an appmock instance to
%% obtain full history of messages received on given endpoint.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_history(Hostname :: binary(), Port :: integer()) ->
    {ok, [binary()]} | {error, term()}.
tcp_server_history(Hostname, Port) ->
    try
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        Path = list_to_binary(?TCP_SERVER_HISTORY_PATH(Port)),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            Path, post, [], <<"">>),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        ?TCP_SERVER_HISTORY_UNPACK_RESPONSE(RespBody)
    catch T:M ->
        ?error("Error in tcp_server_history - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs a request to an appmock instance to check how many clients are connected to given endpoint (by port).
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_connection_count(Hostname :: binary(), Port :: integer()) -> {ok, integer()} | {error, term()}.
tcp_server_connection_count(Hostname, Port) ->
    try
        Binary = ?TCP_SERVER_CONNECTION_COUNT_PACK_REQUEST,
        {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
        Path = list_to_binary(?TCP_SERVER_CONNECTION_COUNT_PATH(Port)),
        {200, _, RespBodyJSON} = appmock_utils:https_request(Hostname, RemoteControlPort,
            Path, post, [], Binary),
        RespBody = appmock_utils:decode_from_json(RespBodyJSON),
        ?TCP_SERVER_CONNECTION_COUNT_UNPACK_RESPONSE(RespBody)
    catch T:M ->
        ?error("Error in tcp_server_connections_count - ~p:~p", [T, M]),
        {error, M}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns when given number of connections are established on given port, or after it timeouts.
%% The AcceptMore flag makes the function succeed when there is the same or more connections than expected.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_wait_for_connections(Hostname :: binary(), Port :: integer(),
    ConnNumber :: integer(), AcceptMore :: boolean(), Timeout :: integer()) -> ok | {error, term()}.
tcp_server_wait_for_connections(Hostname, Port, ConnNumber, AcceptMore, Timeout) ->
    try
        StartingTime = now(),
        CheckConnNum = fun(ThisFun, WaitFor) ->
            case tcp_server_connection_count(Hostname, Port) of
                {ok, Result} when AcceptMore andalso Result >= ConnNumber ->
                    ok;
                {ok, Result} when Result =:= ConnNumber ->
                    ok;
                {error, wrong_endpoint} ->
                    {error, wrong_endpoint};
                _ ->
                    case utils:milliseconds_diff(now(), StartingTime) > Timeout of
                        true ->
                            {error, timeout};
                        false ->
                            timer:sleep(WaitFor),
                            ThisFun(ThisFun, trunc(WaitFor * ?WAIT_INTERVAL_INCREMENT_RATE))
                    end
            end
        end,
        CheckConnNum(CheckConnNum, ?WAIT_STARTING_CHECK_INTERVAL)
    catch T:M ->
        ?error("Error in tcp_server_wait_for_connections - ~p:~p", [T, M]),
        {error, M}
    end.


