%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a ranch handler for TCP server mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_mock_handler).
-behaviour(ranch_protocol).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
-include("appmock_internal.hrl").

%% Ranch API
-export([start_link/4]).
-export([init/4]).

% How long should the process wait for data on socket before it loops again.
-define(POLLING_PERIOD, 100).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Ranch callback, called to spawn a new handler process for a connection.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Ref :: ranch:ref(), Socket :: term(), Transport :: module(), ProtoOpts :: term()) ->
    {ok, ConnectionPid :: pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.


%%--------------------------------------------------------------------
%% @doc
%% Init function will be called by the newly spawned process.
%% @end
%%--------------------------------------------------------------------
-spec init(Ref :: ranch:ref(), Socket :: term(), Transport :: module(), ProtoOpts :: term()) -> term().
init(Ref, Socket, Transport, [Port, Packet, HttpUpgradeMode]) ->
    ok = ranch:accept_ack(Ref),
    {OK, _Closed, _Error} = Transport:messages(),
    tcp_mock_server:report_connection_state(Port, self(), true),
    case HttpUpgradeMode of
        false ->
            init_tcp_server_loop(Socket, Transport, Port, OK, Packet);
        {true, UpgradePath, ProtocolName} ->
            init_http_upgrade_server(Socket, Transport, Port, OK, Packet, UpgradePath, ProtocolName)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Waits for HTTP Upgrade protocol request and switches into binary server mode.
%% @end
%%--------------------------------------------------------------------
-spec init_http_upgrade_server(Socket :: term(), Transport :: module(), Port :: integer(),
    OK :: term(), Packet :: term(), UpgradePath :: binary(), ProtocolName :: binary()) -> ok.
init_http_upgrade_server(Socket, Transport, Port, OK, Packet, UpgradePath, ProtocolName) ->
    Transport:setopts(Socket, [{active, once}]),
    Result = receive
        {OK, Socket, Data} ->
            case verify_protocol_upgrade_request(Data, UpgradePath, ProtocolName) of
                true ->
                    Transport:send(Socket, protocol_upgrade_response(ProtocolName)),
                    ok;
                false ->
                    ?error("Invalid protocol upgrade request: ~p", [Data]),
                    error
            end;
        Other ->
            ?error("Failure awaiting for protocol upgrade: ~p", [Other]),
            error
    after
        timer:minutes(5) ->
            ?error("Timeout while waiting for HTTP upgrade"),
            error
    end,
    case Result of
        ok ->
            init_tcp_server_loop(Socket, Transport, Port, OK, Packet);
        error ->
            tcp_mock_server:report_connection_state(Port, self(), false),
            ok = Transport:close(Socket)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Initializes binary TCP server loop.
%% @end
%%--------------------------------------------------------------------
-spec init_tcp_server_loop(Socket :: term(), Transport :: module(), Port :: integer(),
    OK :: term(), Packet :: term()) -> ok.
init_tcp_server_loop(Socket, Transport, Port, OK, Packet) ->
    Transport:setopts(Socket, [{packet, Packet}]),
    {ok, Timeout} = application:get_env(?APP_NAME, tcp_connection_timeout),
    loop(Socket, Transport, Port, Timeout, OK).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop that handles a TCP connection.
%% TimeoutIn means after how long without receiving any data will the connection be closed.
%% If it reaches zero, the connection is closed. If data is received, its reset to the value from env.
%% @end
%%--------------------------------------------------------------------
-spec loop(Socket :: port(), Transport :: module(), Port :: integer(), TimeoutIn :: integer(), OK :: term()) -> ok.
loop(Socket, Transport, Port, TimeoutIn, _OK) when TimeoutIn < 0 ->
    % The connection timed out, notify the gen_server and close it.
    tcp_mock_server:report_connection_state(Port, self(), false),
    ok = Transport:close(Socket);

loop(Socket, Transport, Port, TimeoutIn, OK) ->
    Now = erlang:monotonic_time(milli_seconds),
    Transport:setopts(Socket, [{active, once}]),
    receive
        {OK, Socket, Data} ->
            % Received some data, save it to history
            tcp_mock_server:register_packet(Port, Data),
            {ok, Timeout} = application:get_env(?APP_NAME, tcp_connection_timeout),
            loop(Socket, Transport, Port, Timeout, OK);

        {ReplyToPid, send, DataToSend, MessageCount} ->
            % There is something to send, send it and
            % inform the requesting process that it succeeded.
            ok = send_messages(Transport, Socket, DataToSend, MessageCount),
            ReplyToPid ! {self(), ok},
            loop(Socket, Transport, Port, TimeoutIn - (erlang:monotonic_time(milli_seconds) - Now), OK);

        _ ->
            % Close the connection
            loop(Socket, Transport, Port, -1, OK)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper functions to send a given number of the same messages via a socket.
%% @end
%%--------------------------------------------------------------------
send_messages(_, _, _, 0) ->
    ok;
send_messages(Transport, Socket, DataToSend, Count) ->
    Transport:send(Socket, DataToSend),
    send_messages(Transport, Socket, DataToSend, Count - 1).


-spec verify_protocol_upgrade_request(Request :: binary(), UpgradePath :: binary(),
    ProtocolName :: binary()) -> boolean().
verify_protocol_upgrade_request(Request, UpgradePath, ProtocolName) ->
    try
        Lines = binary:split(Request, <<"\r\n">>, [global, trim_all]),
        has_member_case_insensitive(<<"GET ", UpgradePath/binary, " HTTP/1.1">>, Lines) andalso
            has_member_case_insensitive(<<"Connection: Upgrade">>, Lines) andalso
            has_member_case_insensitive(<<"Upgrade: ", ProtocolName/binary>>, Lines)
    catch _:_ ->
        false
    end.


-spec has_member_case_insensitive(binary(), [binary()]) -> boolean().
has_member_case_insensitive(_Bin, []) ->
    false;
has_member_case_insensitive(Bin, [First | Rest]) ->
    case string:to_lower(binary_to_list(Bin)) =:= string:to_lower(binary_to_list(First)) of
        true -> true;
        false -> has_member_case_insensitive(Bin, Rest)
    end.


-spec protocol_upgrade_response(ProtocolName :: binary()) -> binary().
protocol_upgrade_response(ProtocolName) -> <<
    "HTTP/1.1 101 Switching Protocols\r\n"
    "Connection: Upgrade\r\n"
    "Upgrade: ", ProtocolName/binary, "\r\n"
    "\r\n"
>>.