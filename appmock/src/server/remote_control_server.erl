%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This gen_server handles all requests connected with remote control
%%% functionalities. Usually, it needs to call other gen_servers to
%%% process the requests.
%%% In addition, it starts and stops the remote control cowboy listener.
%%% @end
%%%-------------------------------------------------------------------
-module(remote_control_server).
-author("Lukasz Opiola").

-behaviour(gen_server).

-include("appmock_internal.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0, healthcheck/0]).
-export([rest_endpoint_request_count/2, verify_rest_mock_history/1]).
-export([tcp_server_message_count/2, tcp_server_send/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {}).

-define(SERVER, ?MODULE).
% Identifier of cowboy listener that handles all remote control requests.
-define(REMOTE_CONTROL_LISTENER, remote_control).
% Number of acceptors in cowboy listeners
-define(NUMBER_OF_ACCEPTORS, 10).

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
%% Returns how many times has an endpoint been requested.
%% This task is delegated straight to rest_mock_server, but this function is here
%% for clear API.
%% @end
%%--------------------------------------------------------------------
-spec rest_endpoint_request_count(Port :: integer(), Path :: binary()) ->
    {ok, integer()} | {error, wrong_endpoint}.
rest_endpoint_request_count(Port, Path) ->
    rest_mock_server:rest_endpoint_request_count(Port, Path).


%%--------------------------------------------------------------------
%% @doc
%% Handles requests to verify if all endpoints have been requested in expected order.
%% This task is delegated straight to rest_mock_server, but this function is here
%% for clear API.
%% @end
%%--------------------------------------------------------------------
-spec verify_rest_mock_history(ExpectedHistory :: PortPathMap) ->
    true | {false, PortPathMap} | {error, term()} when PortPathMap :: [{Port :: integer(), Path :: binary()}].
verify_rest_mock_history(ExpectedHistory) ->
    rest_mock_server:verify_rest_mock_history(ExpectedHistory).


%%--------------------------------------------------------------------
%% @doc
%% Returns how many times has a TCP esrver received specific message.
%% This task is delegated straight to tcp_mock_server, but this function is here
%% for clear API.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_message_count(Port :: integer(), Data :: binary()) -> {ok, integer()} | {error, term()}.
tcp_server_message_count(Port, Data) ->
    tcp_mock_server:tcp_server_message_count(Port, Data).


%%--------------------------------------------------------------------
%% @doc
%% Sends given data to all clients connected to the TCP server on specified port.
%% This task is delegated straight to tcp_mock_server, but this function is here
%% for clear API.
%% @end
%%--------------------------------------------------------------------
-spec tcp_server_send(Port :: integer(), Data :: binary()) -> true | {error, term()}.
tcp_server_send(Port, Data) ->
    tcp_mock_server:tcp_server_send(Port, Data).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    start_remote_control_listener(),
    {ok, #state{}}.

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
handle_call(healthcheck, _From, State) ->
    Reply =
        try
            {ok, RCPort} = application:get_env(?APP_NAME, remote_control_port),
            % Check connectivity to rest endpoint verification path with some random data
            {200, _, _} = appmock_utils:https_request(<<"127.0.0.1">>, RCPort, <<?REST_ENDPOINT_REQUEST_COUNT_PATH>>, get, [],
                <<"{\"port\":10, \"path\":\"/\", \"number\":7}">>),
            % Check connectivity to rest history verification path with some random data
            {200, _, _} = appmock_utils:https_request(<<"127.0.0.1">>, RCPort, <<?VERIFY_REST_HISTORY_PATH>>, get, [],
                <<"{\"endpoint\":{\"port\":8080, \"path\":\"/\"}}">>),
            % Check connectivity to tcp server mock verification path with some random data
            Path = list_to_binary(?TCP_SERVER_MESSAGE_COUNT_PATH(8080)),
            {200, _, _} = appmock_utils:https_request(<<"127.0.0.1">>, RCPort, Path, get, [],
                <<"random_data!%$$^&%^&*%^&*">>),
            ok
        catch T:M ->
            ?error_stacktrace("Error during ~p healthcheck- ~p:~p", [?MODULE, T, M]),
            error
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
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ?info("Stopping cowboy listener: ~p", [?REMOTE_CONTROL_LISTENER]),
    cowboy:stop_listener(?REMOTE_CONTROL_LISTENER),
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
%% Starts a cowboy listener that handles all remote control requests.
%% @end
%%--------------------------------------------------------------------
-spec start_remote_control_listener() -> ok.
start_remote_control_listener() ->
    {ok, RemoteControlPort} = application:get_env(?APP_NAME, remote_control_port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {?NAGIOS_ENPOINT, remote_control_handler, [?NAGIOS_ENPOINT]},
            {?VERIFY_REST_HISTORY_PATH, remote_control_handler, [?VERIFY_REST_HISTORY_PATH]},
            {?REST_ENDPOINT_REQUEST_COUNT_PATH, remote_control_handler, [?REST_ENDPOINT_REQUEST_COUNT_PATH]},
            {?TCP_SERVER_MESSAGE_COUNT_COWBOY_ROUTE, remote_control_handler, [?TCP_SERVER_MESSAGE_COUNT_COWBOY_ROUTE]},
            {?TCP_SERVER_SEND_COWBOY_ROUTE, remote_control_handler, [?TCP_SERVER_SEND_COWBOY_ROUTE]}
        ]}
    ]),
    % Load certificates' paths from env
    {ok, CaCertFile} = application:get_env(?APP_NAME, ca_cert_file),
    {ok, CertFile} = application:get_env(?APP_NAME, cert_file),
    {ok, KeyFile} = application:get_env(?APP_NAME, key_file),
    % Start a https listener on given port
    ?info("Starting cowboy listener: ~p (~p)", [?REMOTE_CONTROL_LISTENER, RemoteControlPort]),
    {ok, _} = cowboy:start_https(
        ?REMOTE_CONTROL_LISTENER,
        ?NUMBER_OF_ACCEPTORS,
        [
            {port, RemoteControlPort},
            {cacertfile, CaCertFile},
            {certfile, CertFile},
            {keyfile, KeyFile}
        ],
        [
            {env, [{dispatch, Dispatch}]}
        ]),
    ok.
