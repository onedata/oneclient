%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This gen_server provides following functionalities:
%%% - loading REST endpoints mocks from description module
%%% - starting and stopping cowboy listeners
%%% - in-memory persistence for state such as the state of endpoints or history of calls.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_mock_server).
-author("Lukasz Opiola").

-behaviour(gen_server).

-include("appmock_internal.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0, healthcheck/0]).
-export([produce_response/2, rest_endpoint_request_count/2, verify_rest_mock_history/1, reset_rest_mock_history/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
% Number of acceptors in cowboy listeners
-define(NUMBER_OF_ACCEPTORS, 10).

% Internal state of the gen server
-record(state, {
    listeners = [] :: [term()],
    request_history = [] :: [{Port :: integer(), Path :: binary()}],
    mock_states = dict:new() :: dict:dict(),
    initial_mock_states = dict:new() :: dict:dict()
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
%% Handles a request on a mocked endpoint. The endpoint is uniquely recognized by a pair {Port, Path}.
%% If the endpoint had a responding function defined, the state is set to the state returned by the function.
%% @end
%%--------------------------------------------------------------------
-spec produce_response(Req :: cowboy_req:req(), ETSKey :: {Port :: integer(), Path :: binary()}) ->
    {ok, {Code :: integer(), Headers :: [{term(), term()}], Body :: binary()}}.
produce_response(Req, ETSKey) ->
    % Extract request body here as it can only be done from the handler process
    {ok, ReqBody, _} = cowboy_req:read_body(Req),
    gen_server:call(?SERVER, {produce_response, Req, ReqBody, ETSKey}).


%%--------------------------------------------------------------------
%% @doc
%% Handles requests to verify if certain endpoint had been requested given amount of times.
%% @end
%%--------------------------------------------------------------------
-spec rest_endpoint_request_count(Port :: integer(), Path :: binary()) ->
    {ok, integer()} | {error, wrong_endpoint}.
rest_endpoint_request_count(Port, Path) ->
    gen_server:call(?SERVER, {rest_endpoint_request_count, Port, Path}).


%%--------------------------------------------------------------------
%% @doc
%% Handles requests to verify if all endpoints have been requested in expected order.
%% @end
%%--------------------------------------------------------------------
-spec verify_rest_mock_history(ExpectedOrder :: PortPathMap) ->
    true | {false, PortPathMap} | {error, term()} when PortPathMap :: [{Port :: integer(), Path :: binary()}].
verify_rest_mock_history(ExpectedOrder) ->
    gen_server:call(?SERVER, {verify_rest_mock_history, ExpectedOrder}).


%%--------------------------------------------------------------------
%% @doc
%% Handles requests to reset ALL mocked REST endpoints.
%% @end
%%--------------------------------------------------------------------
-spec reset_rest_mock_history() -> true.
reset_rest_mock_history() ->
    gen_server:call(?SERVER, reset_rest_mock_history).


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
    Mappings = get_mappings(DescriptionModule),
    StatesDict = convert_mappings_to_states_dict(Mappings),
    ListenersIDs = start_listeners_for_mappings(Mappings),
    {ok, #state{mock_states = StatesDict, listeners = ListenersIDs, initial_mock_states = StatesDict}}.

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
handle_call(healthcheck, _From, #state{mock_states = MockStates} = State) ->
    try
        MockStatesList = dict:to_list(MockStates),
        % Check if all mocked endpoints are connective.
        lists:foreach(
            fun({{Port, Path}, MockState}) ->
                case MockState of
                    #rest_mock_state{response = #rest_response{code = Code}} ->
                        URL = str_utils:format_bin("https://127.0.0.1:~B~s",
                            [Port, Path]),
                        {ok, Code, _, _} = http_client:get(URL);
                    _ ->
                        ok
                end
            end, MockStatesList),

        {reply, ok, State}
    catch T:M ->
        ?error_stacktrace("Error during ~p healthcheck- ~p:~p", [?MODULE, T, M]),
        {reply, error, State}
    end;

handle_call({produce_response, Req, ReqBody, ETSKey}, _From, State) ->
    {{Code, Headers, Body}, NewState} = internal_produce_response(Req, ReqBody, ETSKey, State),
    {reply, {ok, {Code, Headers, Body}}, NewState};

handle_call({rest_endpoint_request_count, Port, Path}, _From, State) ->
    #state{mock_states = MockStatesDict} = State,
    Reply = case dict:find({Port, Path}, MockStatesDict) of
                error ->
                    {error, wrong_endpoint};
                {ok, [#rest_mock_state{counter = ActualNumber}]} ->
                    {ok, ActualNumber}
            end,
    {reply, Reply, State};

handle_call({verify_rest_mock_history, ExpectedHistory}, _From, State) ->
    #state{request_history = ActualHistory} = State,
    Reply = case ExpectedHistory of
                ActualHistory -> true;
                _ -> {false, ActualHistory}
            end,
    {reply, Reply, State};

handle_call(reset_rest_mock_history, _From, State) ->
    #state{initial_mock_states = InitialStatesDict} = State,
    {reply, true, State#state{request_history = [], mock_states = InitialStatesDict}};

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
    % Stop all previously started cowboy listeners
    lists:foreach(
        fun(Listener) ->
            ?info("Stopping cowboy listener: ~p", [Listener]),
            cowboy:stop_listener(Listener)
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
%% Analyses the list of mappings returned by rest_mocks() of the description module
%% and produces a map {Port, Path} -> #mapping_state for every port.
%% @end
%%--------------------------------------------------------------------
-spec get_mappings(ModuleName :: atom()) ->
    [{Port :: integer(), [{{Port :: integer(), Path :: integer()}, #rest_mock_state{}}]}].
get_mappings(ModuleName) ->
    % Get all mappings by calling request_mappings/0 function
    Mappings = ModuleName:rest_mocks(),
    lists:foldl(
        fun(#rest_mock{port = Port, path = Path, response = Resp, initial_state = InitState}, PortsProplist) ->
            EndpointsForPort = proplists:get_value(Port, PortsProplist, []),
            NewEndpoints = [{{Port, Path}, #rest_mock_state{response = Resp, state = InitState}} | EndpointsForPort],
            % Remember the mapping in the proplist
            [{Port, NewEndpoints} | proplists:delete(Port, PortsProplist)]
        end, [], Mappings).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decides on which port should the application listen based on mappings
%% in description module and starts the listeners.
%% @end
%%--------------------------------------------------------------------
-spec convert_mappings_to_states_dict(Mappings) -> dict:dict() when
    Mappings :: [{Port :: integer(), [{{Port :: integer(), Path :: integer()}, #rest_mock_state{}}]}].
convert_mappings_to_states_dict(Mappings) ->
    Dict = dict:new(),
    lists:foldl(
        fun({_, EndpointsForPort}, CurrentDict) ->
            lists:foldl(
                fun({{Port, Path}, #rest_mock_state{} = MockState}, CurrentDictInside) ->
                    dict:append({Port, Path}, MockState, CurrentDictInside)
                end, CurrentDict, EndpointsForPort)
        end, Dict, Mappings).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decides on which port should the application listen based on mappings
%% in description module and starts the listeners.
%% @end
%%--------------------------------------------------------------------
-spec start_listeners_for_mappings(Mappings :: [{Port :: integer(), [{{Port :: integer(), Path :: integer()}, #rest_mock_state{}}]}]) ->
    ListenerIDs :: [term()].
start_listeners_for_mappings(Mappings) ->
    % Create pairs {Port, CowboyDispatch} for every port, where CowboyDispatch
    % includes all endpoints for a certain port
    _ListenerIDs = lists:map(
        fun({Port, EndpointsForPort}) ->
            Dispatch = cowboy_router:compile([
                {'_', lists:map(
                    fun({{MPort, MPath}, _}) ->
                        {binary_to_list(MPath), rest_mock_handler, [{MPort, MPath}]}
                    end, EndpointsForPort)
                }
            ]),
            % Generate listener name
            ListenerID = "https" ++ integer_to_list(Port),
            start_listener(ListenerID, Port, Dispatch),
            ListenerID
        end, Mappings).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts a single cowboy https listener.
%% @end
%%--------------------------------------------------------------------
-spec start_listener(ListenerID :: term(), Port :: integer(), Dispatch :: term()) -> ok.
start_listener(ListenerID, Port, Dispatch) ->
    % Load certificates' paths from env
    {ok, CaCertFile} = application:get_env(?APP_NAME, ca_cert_file),
    {ok, CertFile} = application:get_env(?APP_NAME, cert_file),
    {ok, KeyFile} = application:get_env(?APP_NAME, key_file),
    % Start a https listener on given port
    ?info("Starting cowboy listener: ~p (~p)", [ListenerID, Port]),
    {ok, _} = ranch:start_listener(ListenerID, ranch_ssl,
        [
            {num_acceptors, ?NUMBER_OF_ACCEPTORS},
            {port, Port},
            {cacertfile, CaCertFile},
            {certfile, CertFile},
            {keyfile, KeyFile},
            {next_protocols_advertised, [<<"http/1.1">>]},
            {alpn_preferred_protocols, [<<"http/1.1">>]}
        ],
        cowboy_tls, #{
            env => #{dispatch => Dispatch},
            connection_type => supervisor,
            idle_timeout => infinity,
            inactivity_timeout => timer:hours(24)
        }),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Internal function called from handle_call, creating a response from mocked endpoint.
%% @end
%%--------------------------------------------------------------------
-spec internal_produce_response(Req :: cowboy_req:req(), ReqBody :: binary(), ETSKey :: {Port :: integer(), Path :: binary()}, State :: #state{}) ->
    {{Code :: integer(), Headers :: [{term(), term()}], Body :: binary()}, NewState :: #state{}}.
internal_produce_response(Req, ReqBody, ETSKey, #state{request_history = History, mock_states = MockStatesDict} = ServerState) ->
    % Get the response term and current state by {Port, Path} key
    {ok, [MockStateRec]} = dict:find(ETSKey, MockStatesDict),
    #rest_mock_state{response = ResponseField, state = MockState, counter = Counter} = MockStateRec,
    % Get response and new state - either directly, cyclically from a list or by evaluating a fun
    {Response, NewMockState} =
        case ResponseField of
            #rest_response{} ->
                {ResponseField, MockState};
            RespList when is_list(RespList) ->
                CurrentResp = lists:nth((Counter rem length(RespList)) + 1, RespList),
                {CurrentResp, MockState};
            Fun when is_function(Fun, 3) ->
                {_Response, _NewState} = Fun(Req, ReqBody, MockState)
        end,
    % Put new state in the dictionary
    NewMockStatesDict = dict:update(ETSKey,
        fun(_) ->
            [MockStateRec#rest_mock_state{state = NewMockState, counter = Counter + 1}]
        end, MockStatesDict),
    #rest_response{code = Code, body = Body, content_type = CType, headers = Headers} = Response,
    AllHeaders = [{<<"content-type">>, CType}] ++ Headers,
    {Port, Path} = ETSKey,
    ?info("Got request at :~p~s~nResponding~n  Code:    ~p~n  Headers: ~p~n  Body:    ~s", [Port, Path, Code, AllHeaders, Body]),
    {{Code, AllHeaders, Body}, ServerState#state{request_history = History ++ [ETSKey], mock_states = NewMockStatesDict}}.
