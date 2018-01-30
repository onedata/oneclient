%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This header file contains record definitions and macros used in
%%% mock app description files.
%%% @end
%%%-------------------------------------------------------------------

%%%===================================================================
%%% Mocking responses
%%%===================================================================

% This record represents a single response returned by a mocked endpoint.
-record(rest_response, {
    code = 200 :: integer(),
    content_type = <<"application/json">>,
    headers = [],
    body = <<"">>
}).

% This record describes a single mapping, uniquely distinguished by port and path.
% It's used to mock HTTP endpoints that will return predefined or dynamically generated answers.
-record(rest_mock, {
    % port on which requests will be accepted
    port = 443 :: integer(),
    % cowboy_router compatible path on which requests will be accepted
    path = <<"/">> :: binary(),
    % response can be:
    % 1) explicit #mock_resp record, will be returned every time the endpoint is used
    % 2) a list of #mock_resp records that will be returned in given sequence, cyclically
    % 3) a 2 argument function:
    %       fun(Req, State) -> {#mock_resp{}, NewState}
    %       Req - cowboy #req{} record, read only
    %       State - carries state between consecutive requests on the same stub
    response = #rest_response{} :: #rest_response{} | [#rest_response{}] | function(),
    % initial state of the stub
    initial_state = [] :: term()
}).

% This record represents a TCP server mock.
-record(tcp_server_mock, {
    port = 5555,
    ssl = true,
    % Erlang transport's packet option that will be passed to server initialization.
    packet = raw,
    % If http_upgrade_mode is disabled, this is a bare tcp server.
    % If http_upgrade_mode is enabled, the server starts up as a HTTP server
    % and expects a valid HTTP Upgrade request (GET with "Connection: Upgrade"
    % headers). Upon such message, it sends back a 101 Switching Protocol
    % response and switches into binary mode. Desired "packet" option is set
    % after the protocol upgrade (up to that point it is set to 'raw').
    %   UpgradePath - URN where the upgrade should be performed, e.g. <<"/clproto">>
    %   ProtocolName - name of the protocol. e.g. <<"clproto">>
    http_upgrade_mode = false :: false | {true, UpgradePath :: binary(), ProtocolName :: binary()},
    % TCP mock can work in two modes:
    % history - it remembers exact history of incoming requests and can validate requests per message contents.
    %    This mode is slow and dedicated for content verification rather that tests with many messages.
    % counter - the endpoint will ignore the content of incoming requests and only count them.
    %    This mode is as fast as it gets.
    % NOTE: in history mode, it is also possible to check the count of all received requests.
    type = history :: counter | history
}).