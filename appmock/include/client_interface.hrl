%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This header file contains record definitions and macros used in
%%% appmock client to interface with remote control endpoint.
%%% @end
%%%-------------------------------------------------------------------

% Term that is sent back when an operation has completed successfully.
-define(TRUE_RESULT, [{<<"result">>, true}]).

-define(NAGIOS_ENPOINT, "/nagios").

%%--------------------------------------------------------------------
% Endpoint used to verify if all mocked endpoint were requested in proper order.
-define(VERIFY_REST_HISTORY_PATH, "/verify_rest_history").

% Transform a proplist of pairs {Port, Path} into a term that is sent as JSON
% to verify_rest_history endpoint (client side).
-define(VERIFY_REST_HISTORY_PACK_REQUEST(_VerificationList),
    lists:map(
        fun({_Port, _Path}) ->
            [{<<"endpoint">>, [{<<"port">>, _Port}, {<<"path">>, _Path}]}]
        end, _VerificationList)
).

% Transform a struct obtained by decoding JSON into a proplist
% of pairs {Port, Path} (server side).
-define(VERIFY_REST_HISTORY_UNPACK_REQUEST(_Struct),
    lists:map(
        fun([{<<"endpoint">>, _Props}]) ->
            {
                proplists:get_value(<<"port">>, _Props),
                proplists:get_value(<<"path">>, _Props)
            }
        end, _Struct)
).

% Produces an error message if verification fails (server side).
-define(VERIFY_REST_HISTORY_PACK_ERROR(_History),
    [
        {<<"result">>, <<"error">>},
        {<<"history">>, ?VERIFY_REST_HISTORY_PACK_REQUEST(_History)}
    ]).

% Retrieves the error details from verify_rest_history error
% (actual request history) (client side).
-define(VERIFY_REST_HISTORY_UNPACK_ERROR(_RespBody),
    begin
        [{<<"result">>, <<"error">>}, {<<"history">>, _Struct}] = _RespBody,
        ?VERIFY_REST_HISTORY_UNPACK_REQUEST(_Struct)
    end
).


%%--------------------------------------------------------------------
% Endpoint used to reset mocked rest endpoint history.
-define(RESET_REST_HISTORY_PATH, "/reset_rest_history").


%%--------------------------------------------------------------------
% Endpoint used to retrieve the number of requests on given REST path.
-define(REST_ENDPOINT_REQUEST_COUNT_PATH, "/rest_endpoint_request_count").

% Creates a term that is sent as JSON to
% verify_rest_endpoint endpoint (client side).
-define(REST_ENDPOINT_REQUEST_COUNT_REQUEST(_Port, _Path),
    [
        {<<"port">>, _Port},
        {<<"path">>, _Path}
    ]
).

% Retrieves params sent to verify_rest_endpoint endpoint (server side).
-define(REST_ENDPOINT_REQUEST_COUNT_UNPACK_REQUEST(_Struct),
    {
        proplists:get_value(<<"port">>, _Struct),
        proplists:get_value(<<"path">>, _Struct)
    }
).

% Produces success message which carries information of message count.
-define(REST_ENDPOINT_REQUEST_COUNT_PACK_RESPONSE(_Count),
    [{<<"result">>, _Count}]
).

% Produces an error message if the endpoint requested
% to be verified does not exist (server side).
-define(REST_ENDPOINT_REQUEST_COUNT_PACK_ERROR_WRONG_ENDPOINT,
    [{<<"result">>, <<"error">>}, {<<"reason">>, <<"wrong_endpoint">>}]).

% Retrieves the error details from verify_rest_endpoint error (client side).
% Retrieves the response from appmock server (client side).
-define(REST_ENDPOINT_REQUEST_COUNT_UNPACK_RESPONSE(_RespBody),
    case _RespBody of
        [{<<"result">>, _Count}] -> {ok, _Count};
        [{<<"result">>, <<"error">>}, {<<"reason">>, <<"wrong_endpoint">>}] ->
            {error, wrong_endpoint}
    end
).


%%--------------------------------------------------------------------
% Endpoint used to verify if a mocked TCP server has received a given packet.
% The port binding is used to identify the TCP server.
-define(TCP_SERVER_SPECIFIC_MESSAGE_COUNT_PATH(_Port),
    "/tcp_server_specific_message_count/" ++ integer_to_list(_Port)).
-define(TCP_SERVER_SPECIFIC_MESSAGE_COUNT_COWBOY_ROUTE,
    "/tcp_server_specific_message_count/:port").

% Creates message that is sent to
% tcp_server_message_count endpoint (client side).
-define(TCP_SERVER_SPECIFIC_MESSAGE_COUNT_PACK_REQUEST(_BinaryData),
    base64:encode(_BinaryData)
).

% Retrieves data sent to tcp_server_message_count endpoint (server side).
-define(TCP_SERVER_SPECIFIC_MESSAGE_COUNT_UNPACK_REQUEST(_BinaryData),
    base64:decode(_BinaryData)
).

% Produces success message which carries information of request count.
-define(TCP_SERVER_SPECIFIC_MESSAGE_COUNT_PACK_RESPONSE(_Count),
    [{<<"result">>, _Count}]
).

% Produces an error message if the tcp server
% requested to be verified does not exist (server side).
-define(TCP_SERVER_SPECIFIC_MESSAGE_COUNT_PACK_ERROR_WRONG_ENDPOINT,
    [{<<"result">>, <<"error">>}, {<<"reason">>, <<"wrong_endpoint">>}]).

% Retrieves the response from appmock server (client side).
-define(TCP_SERVER_SPECIFIC_MESSAGE_COUNT_UNPACK_RESPONSE(_RespBody),
    case _RespBody of
        [{<<"result">>, _Count}] -> {ok, _Count};
        [{<<"result">>, <<"error">>}, {<<"reason">>, <<"wrong_endpoint">>}] ->
            {error, wrong_endpoint}
    end
).


%%--------------------------------------------------------------------
% Endpoint used to check number of all received requests on a TCP mock endpoint.
% The port binding is used to identify the TCP server.
-define(TCP_SERVER_ALL_MESSAGES_COUNT_PATH(_Port),
    "/tcp_server_all_messages_count/" ++ integer_to_list(_Port)).
-define(TCP_SERVER_ALL_MESSAGES_COUNT_COWBOY_ROUTE,
    "/tcp_server_all_messages_count/:port").

% Produces success message which carries information of request count.
-define(TCP_SERVER_ALL_MESSAGES_COUNT_PACK_RESPONSE(_Count),
    [{<<"result">>, _Count}]
).

% Produces an error message if the tcp server
%% requested to be verified does not exist (server side).
-define(TCP_SERVER_ALL_MESSAGES_COUNT_PACK_ERROR_WRONG_ENDPOINT,
    [{<<"result">>, <<"error">>}, {<<"reason">>, <<"wrong_endpoint">>}]).

% Retrieves the response from appmock server (client side).
-define(TCP_SERVER_ALL_MESSAGES_COUNT_UNPACK_RESPONSE(_RespBody),
    case _RespBody of
        [{<<"result">>, _Count}] -> {ok, _Count};
        [{<<"result">>, <<"error">>}, {<<"reason">>, <<"wrong_endpoint">>}] ->
            {error, wrong_endpoint}
    end
).


%%--------------------------------------------------------------------
% Endpoint used to send given data to all clients connected to specified server.
% The port binding is used to identify the TCP server.
-define(TCP_SERVER_SEND_PATH(_Port, _MessageCount),
    str_utils:format("/tcp_server_send/~b/~b", [_Port, _MessageCount])
).
-define(TCP_SERVER_SEND_COWBOY_ROUTE, "/tcp_server_send/:port/:count").

% Creates message that is sent to tcp_server_send endpoint (client side),
% given amount of times.
-define(TCP_SERVER_SEND_PACK_REQUEST(_BinaryData),
    base64:encode(_BinaryData)
).

% Retrieves data sent to tcp_server_send endpoint (server side).
-define(TCP_SERVER_SEND_UNPACK_REQUEST(_BinaryData),
    base64:decode(_BinaryData)
).

% Produces an error message if sending fails.
-define(TCP_SERVER_SEND_PACK_SEND_FAILED_ERROR,
    [{<<"result">>, <<"error">>}, {<<"reason">>, <<"failed_to_send_data">>}]).

% Produces an error message if the tcp server
% requested to send data does not exist (server side).
-define(TCP_SERVER_SEND_PACK_WRONG_ENDPOINT_ERROR,
    [{<<"result">>, <<"error">>}, {<<"reason">>, <<"wrong_endpoint">>}]).

% Retrieves the error details from tcp_server_send error (client side).
-define(TCP_SERVER_SEND_UNPACK_ERROR(_RespBody),
    case _RespBody of
        [
            {<<"result">>, <<"error">>},
            {<<"reason">>, <<"failed_to_send_data">>}
        ] ->
            {error, failed_to_send_data};
        [
            {<<"result">>, <<"error">>},
            {<<"reason">>, <<"wrong_endpoint">>}
        ] ->
            {error, wrong_endpoint}
    end
).


%%--------------------------------------------------------------------
% Endpoint used to obrain message history for given port.
-define(TCP_SERVER_HISTORY_PATH(_Port),
    "/tcp_server_history/" ++ integer_to_list(_Port)).
-define(TCP_SERVER_HISTORY_COWBOY_ROUTE,
    "/tcp_server_history/:port").

% Produces success message which carries information of message history.
-define(TCP_SERVER_HISTORY_PACK_RESPONSE(_Messages),
    [{<<"result">>, [base64:encode(M) || M <- _Messages]}]
).

% Produces an error message if the tcp server
% requested to be verified does not exist (server side).
-define(TCP_SERVER_HISTORY_PACK_ERROR_WRONG_ENDPOINT,
    [
        {<<"result">>, <<"error">>},
        {<<"reason">>, <<"wrong_endpoint">>}
    ]).

% Produces an error message if the tcp server
% requested to be verified works in counter mode.
-define(TCP_SERVER_HISTORY_PACK_ERROR_COUNTER_MODE,
    [
        {<<"result">>, <<"error">>},
        {<<"reason">>, <<"counter_mode">>}
    ]).

% Retrieves the response from appmock server (client side).
-define(TCP_SERVER_HISTORY_UNPACK_RESPONSE(_RespBody),
    case _RespBody of
        [{<<"result">>, _Messages}] ->
            {ok, [base64:decode(M) || M <- _Messages]};
        [{<<"result">>, <<"error">>}, {<<"reason">>, <<"wrong_endpoint">>}] ->
            {error, wrong_endpoint};
        [{<<"result">>, <<"error">>}, {<<"reason">>, <<"counter_mode">>}] ->
            {error, counter_mode}
    end
).


%%--------------------------------------------------------------------
% Endpoint used to reset mocked TCP endpoint history.
-define(RESET_TCP_SERVER_HISTORY_PATH, "/reset_tcp_server_history").


%%--------------------------------------------------------------------
% Endpoint used to check number of client connections.
-define(TCP_SERVER_CONNECTION_COUNT_PATH(_Port),
    "/tcp_server_connection_count/" ++ integer_to_list(_Port)).
-define(TCP_SERVER_CONNECTION_COUNT_COWBOY_ROUTE,
    "/tcp_server_connection_count/:port").

% No data is required to be sent
-define(TCP_SERVER_CONNECTION_COUNT_PACK_REQUEST, []).

% Produces success message which carries information of connection count.
-define(TCP_SERVER_CONNECTION_COUNT_PACK_RESPONSE(_Count),
    [{<<"result">>, _Count}]
).

% Produces an error message if the tcp server
% requested to be verified does not exist (server side).
-define(TCP_SERVER_CONNECTION_COUNT_PACK_ERROR_WRONG_ENDPOINT,
    [
        {<<"result">>, <<"error">>},
        {<<"reason">>, <<"wrong_endpoint">>}
    ]).

% Retrieves the response from appmock server (client side).
-define(TCP_SERVER_CONNECTION_COUNT_UNPACK_RESPONSE(_RespBody),
    case _RespBody of
        [{<<"result">>, _Count}] -> {ok, _Count};
        [{<<"result">>, <<"error">>}, {<<"reason">>, <<"wrong_endpoint">>}] ->
            {error, wrong_endpoint}
    end
).


