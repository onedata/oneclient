%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a cowboy handler and processes remote control requests.
%%% @end
%%%-------------------------------------------------------------------
-module(remote_control_handler).
-behaviour(cowboy_handler).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
-include("appmock_internal.hrl").

%% Cowboy API
-export([init/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Cowboy callback handling the request.
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), Args :: term()) -> {ok, cowboy_req:req(), Path :: string()}.
init(Req, State) ->
    % The request state is it's path, so we can easily create cases for handle function.
    [Path] = State,
    Req2 = cowboy_req:set_resp_header(<<"connection">>, <<"close">>, Req),
    % This is a REST endpoint, close connection after every request.
    Req3 =
        try
            handle_request(Path, Req2)
        catch T:M ->
            ?error_stacktrace("Error in remote_control_handler. Path: ~p. ~p:~p.",
                [Path, T, M]),
            cowboy_req:reply(500, Req2)
        end,
    {ok, Req3, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Processing of different requests.
%% It decodes a request, delegates the logic to remote_control_server and encodes the answer.
%% @end
%%--------------------------------------------------------------------
-spec handle_request(Path :: term(), Req :: cowboy_req:req()) -> NewReq :: cowboy_req:req().
handle_request(?NAGIOS_ENPOINT, Req) ->
    HealthcheckResponses = [
        rest_mock_server:healthcheck(),
        remote_control_server:healthcheck(),
        tcp_mock_server:healthcheck()
    ],

    AppStatus = case lists:duplicate(length(HealthcheckResponses), ok) of
                    HealthcheckResponses -> ok;
                    _ -> error
                end,

    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(erlang:timestamp()),
    DateString = str_utils:format("~4..0w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w", [YY, MM, DD, Hour, Min, Sec]),
    Healthdata = {healthdata, [{date, DateString}, {status, atom_to_list(AppStatus)}], []},
    Content = lists:flatten([Healthdata]),
    Export = xmerl:export_simple(Content, xmerl_xml),
    Reply = io_lib:format("~s", [lists:flatten(Export)]),

    % Send the reply
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/xml">>}, Reply, Req);

handle_request(?REST_ENDPOINT_REQUEST_COUNT_PATH, Req) ->
    % Unpack the request, getting port and path
    {ok, JSONBody, _} = cowboy_req:read_body(Req),
    Body = json_utils:decode(JSONBody),
    {Port, Path} = ?REST_ENDPOINT_REQUEST_COUNT_UNPACK_REQUEST(Body),
    % Verify the endpoint and return the result encoded to JSON.
    ReplyTerm = case remote_control_server:rest_endpoint_request_count(Port, Path) of
                    {ok, Count} ->
                        ?REST_ENDPOINT_REQUEST_COUNT_PACK_RESPONSE(Count);
                    {error, wrong_endpoint} ->
                        ?REST_ENDPOINT_REQUEST_COUNT_PACK_ERROR_WRONG_ENDPOINT
                end,
    Req2 = cowboy_req:set_resp_body(json_utils:encode(ReplyTerm), Req),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req2);

handle_request(?VERIFY_REST_HISTORY_PATH, Req) ->
    {ok, JSONBody, _} = cowboy_req:read_body(Req),
    BodyStruct = json_utils:decode(JSONBody),
    History = ?VERIFY_REST_HISTORY_UNPACK_REQUEST(BodyStruct),
    % Verify the history and return the result encoded to JSON.
    ReplyTerm = case remote_control_server:verify_rest_mock_history(History) of
                    true ->
                        ?TRUE_RESULT;
                    {false, ActualHistory} ->
                        ?VERIFY_REST_HISTORY_PACK_ERROR(ActualHistory)
                end,
    Req2 = cowboy_req:set_resp_body(json_utils:encode(ReplyTerm), Req),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req2);

handle_request(?RESET_REST_HISTORY_PATH, Req) ->
    ReplyTerm = case remote_control_server:reset_rest_mock_history() of
                    true ->
                        ?TRUE_RESULT
                end,
    Req2 = cowboy_req:set_resp_body(json_utils:encode(ReplyTerm), Req),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req2);

handle_request(?TCP_SERVER_SPECIFIC_MESSAGE_COUNT_COWBOY_ROUTE, Req) ->
    PortBin = req:binding(port, Req),
    Port = binary_to_integer(PortBin),
    % Unpack the request,
    {ok, BodyRaw, _} = cowboy_req:read_body(Req),
    Data = ?TCP_SERVER_SPECIFIC_MESSAGE_COUNT_UNPACK_REQUEST(BodyRaw),
    ReplyTerm = case remote_control_server:tcp_server_specific_message_count(Port, Data) of
                    {ok, Count} ->
                        ?TCP_SERVER_SPECIFIC_MESSAGE_COUNT_PACK_RESPONSE(Count);
                    {error, wrong_endpoint} ->
                        ?TCP_SERVER_SPECIFIC_MESSAGE_COUNT_PACK_ERROR_WRONG_ENDPOINT
                end,
    Req2 = cowboy_req:set_resp_body(json_utils:encode(ReplyTerm), Req),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req2);


handle_request(?TCP_SERVER_ALL_MESSAGES_COUNT_COWBOY_ROUTE, Req) ->
    PortBin = req:binding(port, Req),
    Port = binary_to_integer(PortBin),
    ReplyTerm = case remote_control_server:tcp_server_all_messages_count(Port) of
                    {ok, Count} ->
                        ?TCP_SERVER_ALL_MESSAGES_COUNT_PACK_RESPONSE(Count);
                    {error, wrong_endpoint} ->
                        ?TCP_SERVER_ALL_MESSAGES_COUNT_PACK_ERROR_WRONG_ENDPOINT
                end,
    Req2 = cowboy_req:set_resp_body(json_utils:encode(ReplyTerm), Req),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req2);


handle_request(?TCP_SERVER_SEND_COWBOY_ROUTE, Req) ->
    PortBin = req:binding(port, Req),
    Port = binary_to_integer(PortBin),
    CountBin = req:binding(count, Req),
    Count = binary_to_integer(CountBin),
    % Unpack the request,
    {ok, BodyRaw, _} = cowboy_req:read_body(Req),
    Data = ?TCP_SERVER_SEND_UNPACK_REQUEST(BodyRaw),
    ReplyTerm = case remote_control_server:tcp_server_send(Port, Data, Count) of
                    true ->
                        ?TRUE_RESULT;
                    {error, failed_to_send_data} ->
                        ?TCP_SERVER_SEND_PACK_SEND_FAILED_ERROR;
                    {error, wrong_endpoint} ->
                        ?TCP_SERVER_SEND_PACK_WRONG_ENDPOINT_ERROR
                end,
    Req2 = cowboy_req:set_resp_body(json_utils:encode(ReplyTerm), Req),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req2);

handle_request(?TCP_SERVER_HISTORY_COWBOY_ROUTE, Req) ->
    PortBin = req:binding(port, Req),
    Port = binary_to_integer(PortBin),
    ReplyTerm = case remote_control_server:tcp_mock_history(Port) of
                    {ok, Messages} ->
                        ?TCP_SERVER_HISTORY_PACK_RESPONSE(Messages);
                    {error, wrong_endpoint} ->
                        ?TCP_SERVER_HISTORY_PACK_ERROR_WRONG_ENDPOINT;
                    {error, counter_mode} ->
                        ?TCP_SERVER_HISTORY_PACK_ERROR_COUNTER_MODE
                end,
    Req2 = cowboy_req:set_resp_body(json_utils:encode(ReplyTerm), Req),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req2);

handle_request(?RESET_TCP_SERVER_HISTORY_PATH, Req) ->
    ReplyTerm = case remote_control_server:reset_tcp_mock_history() of
                    true ->
                        ?TRUE_RESULT
                end,
    Req2 = cowboy_req:set_resp_body(json_utils:encode(ReplyTerm), Req),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req2);

handle_request(?TCP_SERVER_CONNECTION_COUNT_COWBOY_ROUTE, Req) ->
    PortBin = req:binding(port, Req),
    Port = binary_to_integer(PortBin),
    ReplyTerm = case remote_control_server:tcp_server_connection_count(Port) of
                    {ok, Count} ->
                        ?TCP_SERVER_CONNECTION_COUNT_PACK_RESPONSE(Count);
                    {error, wrong_endpoint} ->
                        ?TCP_SERVER_CONNECTION_COUNT_PACK_ERROR_WRONG_ENDPOINT
                end,
    Req2 = cowboy_req:set_resp_body(json_utils:encode(ReplyTerm), Req),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Req2).
