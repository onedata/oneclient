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
start_link(Ref, Socket, Transport, [Port] = Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    tcp_mock_server:report_connection_state(Port, Pid, true),
    {ok, Pid}.


%%--------------------------------------------------------------------
%% @doc
%% Init function will be called by the newly spawned process.
%% @end
%%--------------------------------------------------------------------
-spec init(Ref :: ranch:ref(), Socket :: term(), Transport :: module(), ProtoOpts :: term()) -> term().
init(Ref, Socket, Transport, [Port]) ->
    ok = ranch:accept_ack(Ref),
    {ok, Timeout} = application:get_env(?APP_NAME, tcp_connection_timeout),
    loop(Socket, Transport, Port, Timeout).


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
-spec loop(Socket :: port(), Transport :: module(), Port :: integer(), TimeoutIn :: integer()) -> ok.
loop(Socket, Transport, Port, TimeoutIn) when TimeoutIn < 0 ->
    % The connection timed out, notify the gen_server and close it.
    tcp_mock_server:report_connection_state(Port, self(), false),
    ok = Transport:close(Socket);

loop(Socket, Transport, Port, TimeoutIn) ->
    Now = now(),
    case Transport:recv(Socket, 0, ?POLLING_PERIOD) of
        {ok, Data} ->
            % Received some data, save it to history
            tcp_mock_server:register_packet(Port, Data),
            {ok, Timeout} = application:get_env(?APP_NAME, tcp_connection_timeout),
            loop(Socket, Transport, Port, Timeout);

        {error, timeout} ->
            % No data came within the timeout period, check if there is something
            % to send to the client and loop again.
            receive
                {ReplyToPid, send, DataToSend} ->
                    % There is something to send, send it and
                    % inform the requesting process that it succeeded.
                    Transport:send(Socket, DataToSend),
                    ReplyToPid ! {self(), ok}
            after 0 ->
                ok
            end,
            loop(Socket, Transport, Port, TimeoutIn - (timer:now_diff(now(), Now) div 1000));

        _ ->
            % Close the connection
            loop(Socket, Transport, Port, -1)
    end.
