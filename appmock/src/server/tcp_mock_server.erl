%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This gen_server provides following functionalities:
%%% - loading TCP server mocks from description module
%%% - starting and stopping ranch listeners
%%% - in-memory persistence for state such as history of received packets.
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_mock_server).
-author("Lukasz Opiola").

-behaviour(gen_server).

-include("appmock_internal.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0, healthcheck/0]).
-export([report_connection_state/3, register_packet/2, tcp_server_message_count/2, tcp_server_send/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
% Number of acceptors in ranch listeners
-define(NUMBER_OF_ACCEPTORS, 10).
% Timeout of tcp_server_send function - if by this time all connection pids do not report
% back, the sending is considered failed.
-define(SEND_TIMEOUT, 500).

% Internal state of the gen server
-record(state, {
    listeners = [] :: [term()],
    % The history dict holds mappings Packet -> boolean(), where the
    % boolean value means if given packet was received.
    request_history = [] :: [{Port :: integer(), History :: dict:dict()}],
    % The connections proplist holds a list of active pids for each port.
    connections = [] :: [{Port :: integer(), [pid()]}]
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Should check if this gen_server and all underlying services (like cowboy listeners)
%% are ready and working properly. If any error occurs, it should be logged inside this function.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | error.
healthcheck() ->
    {ok, Timeout} = application:get_env(?APP_NAME, nagios_healthcheck_timeout),
    gen_server:call(?SERVER, healthcheck, Timeout).


%%--------------------------------------------------------------------
%% @doc
%% Called from connection pids to register and unregister a connection. Thanks to this,
%% this server can easily order the pids to send some data to clients.
%% @end
%%--------------------------------------------------------------------
-spec report_connection_state(Port :: integer(), Pid :: pid(), IsAlive :: boolean()) -> ok.
report_connection_state(Port, Pid, IsAlive) ->
    gen_server:call(?SERVER, {report_connection_state, Port, Pid, IsAlive}).


%%--------------------------------------------------------------------
%% @doc
%% Saves in history that a certain packet has been received on given port.
%% @end
%%--------------------------------------------------------------------
-spec register_packet(Port :: integer(), Data :: binary()) -> ok.
register_packet(Port, Data) ->
    gen_server:call(?SERVER, {register_packet, Port, Data}).


%%--------------------------------------------------------------------
%% @doc
%% Returns how many times has a TCP esrver received specific message.
%% This task is delegated straight to tcp_mock_server, but this function is here
%% for clear API.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_message_count(Port :: integer(), Data :: binary()) -> {ok, integer()} | {error, term()}.
tcp_server_message_count(Port, Data) ->
    gen_server:call(?SERVER, {tcp_server_message_count, Port, Data}).


%%--------------------------------------------------------------------
%% @doc
%% Sends given data to all clients connected to the TCP server on specified port.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_send(Port :: integer(), Data :: binary()) -> true | {error, term()}.
tcp_server_send(Port, Data) ->
    gen_server:call(?SERVER, {tcp_server_send, Port, Data}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the appmock application by creating an ETS table, initializing records in it,
%% loading given mock app description module and starting cowboy listenera.
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, AppDescriptionFile} = application:get_env(?APP_NAME, app_description_file),
    DescriptionModule = appmock_utils:load_description_module(AppDescriptionFile),
    {ListenersIDs, Ports} = start_listeners(DescriptionModule),
    InitializedHistory = lists:map(
        fun(Port) ->
            {Port, dict:new()}
        end, Ports),
    InitializedConnections = lists:map(
        fun(Port) ->
            {Port, []}
        end, Ports),
    {ok, #state{listeners = ListenersIDs, request_history = InitializedHistory,
        connections = InitializedConnections}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(healthcheck, _From, #state{request_history = RequestHistory} = State) ->
    Reply =
        try
            % Check connectivity to all TCP listeners
            lists:foreach(
                fun({Port, _}) ->
                    {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, []),
                    gen_tcp:close(Socket)
                end, RequestHistory),
            ok
        catch T:M ->
            ?error_stacktrace("Error during ~p healthcheck- ~p:~p", [?MODULE, T, M]),
            error
        end,
    {reply, Reply, State};


handle_call({report_connection_state, Port, Pid, IsAlive}, _From, State) ->
    #state{connections = Connections} = State,
    ConnectionsForPort = proplists:get_value(Port, Connections, []),
    NewConnectionsForPort = case IsAlive of
                                true -> [Pid | ConnectionsForPort];
                                false -> lists:delete(Pid, ConnectionsForPort)
                            end,
    NewConnections = [{Port, NewConnectionsForPort} | proplists:delete(Port, Connections)],
    {reply, ok, State#state{connections = NewConnections}};


handle_call({register_packet, Port, Data}, _From, State) ->
    #state{request_history = RequestHistory} = State,
    HistoryForPort = proplists:get_value(Port, RequestHistory, dict:new()),
    NewHistoryForPort = dict:update(Data, fun([Old]) -> [Old + 1] end, [1], HistoryForPort),
    NewHistory = [{Port, NewHistoryForPort} | proplists:delete(Port, RequestHistory)],
    {reply, ok, State#state{request_history = NewHistory}};

handle_call({tcp_server_message_count, Port, Data}, _From, State) ->
    #state{request_history = RequestHistory} = State,
    HistoryForPort = proplists:get_value(Port, RequestHistory, undefined),
    Reply = case HistoryForPort of
                undefined ->
                    {error, wrong_endpoint};
                _ ->
                    case dict:find(Data, HistoryForPort) of
                        {ok, [Count]} ->
                            {ok, Count};
                        error ->
                            {ok, 0}
                    end
            end,
    {reply, Reply, State};

handle_call({tcp_server_send, Port, Data}, _From, State) ->
    #state{connections = Connections} = State,
    ConnectionsForPort = proplists:get_value(Port, Connections, undefined),
    Reply = case ConnectionsForPort of
                undefined ->
                    {error, wrong_endpoint};
                _ ->
                    Result = utils:pmap(
                        fun(Pid) ->
                            Pid ! {self(), send, Data},
                            receive
                                {Pid, ok} -> ok
                            after
                                ?SEND_TIMEOUT -> error
                            end
                        end, ConnectionsForPort),
                    % If all pids reported back, sending succeded
                    case lists:duplicate(length(Result), ok) of
                        Result ->
                            true;
                        _ ->
                            {error, failed_to_send_data}
                    end
            end,

    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% Cleans up by stopping previously started cowboy listeners and deleting the ETS table.
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{listeners = Listeners}) ->
    % Stop all previously started ranch listeners
    lists:foreach(
        fun(Listener) ->
            ?info("Stopping ranch listener: ~p", [Listener]),
            ranch:stop_listener(Listener)
        end, Listeners),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts all TCP servers that were specified in the app description module.
%% Returns a list of lsitener IDs and a list of ports on which servers have been started.
%% @end
%%--------------------------------------------------------------------
-spec start_listeners(AppDescriptionModule :: module()) ->
    {ListenerIDs :: [term()], Ports :: [integer()]}.
start_listeners(AppDescriptionModule) ->
    TCPServerMocks = AppDescriptionModule:tcp_server_mocks(),
    ListenerIDsAndPorts = lists:map(
        fun(#tcp_server_mock{port = Port, ssl = UseSSL, packet = Packet}) ->
            % Generate listener name
            ListenerID = "tcp" ++ integer_to_list(Port),
            Protocol = case UseSSL of
                           true -> ranch_ssl;
                           false -> ranch_tcp
                       end,
            Opts = case UseSSL of
                       true ->
                           {ok, CaCertFile} = application:get_env(?APP_NAME, ca_cert_file),
                           {ok, CertFile} = application:get_env(?APP_NAME, cert_file),
                           {ok, KeyFile} = application:get_env(?APP_NAME, key_file),
                           [
                               {port, Port},
                               {cacertfile, CaCertFile},
                               {certfile, CertFile},
                               {keyfile, KeyFile}
                           ];
                       false ->
                           [{port, Port}]
                   end,
            {ok, _} = ranch:start_listener(ListenerID, ?NUMBER_OF_ACCEPTORS,
                Protocol, Opts, tcp_mock_handler, [Port, Packet]),
            {ListenerID, Port}
        end, TCPServerMocks),
    {_ListenerIDs, _Ports} = lists:unzip(ListenerIDsAndPorts).
