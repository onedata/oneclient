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
init(Ref, Socket, Transport, [Port, Packet]) ->
    ok = ranch:accept_ack(Ref),
    {ok, Timeout} = application:get_env(?APP_NAME, tcp_connection_timeout),
    Transport:setopts(Socket, [{packet, Packet}]),
    {OK, _Closed, _Error} = Transport:messages(),
    tcp_mock_server:report_connection_state(Port, self(), true),
    loop(Socket, Transport, Port, Timeout, OK).


%%%===================================================================
%%% Internal functions
%%%===================================================================

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
