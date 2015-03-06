%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This header file contains record definitions and macros used internally
%%% in appmock engine.
%%% @end
%%%-------------------------------------------------------------------

-include("appmock.hrl").
-include("client_interface.hrl").

-define(APP_NAME, appmock).

% Records that holds the state of a REST mock in ETS
-record(rest_mock_state, {
    response = #rest_response{} :: #rest_response{} | [#rest_response{}] | function(),
    % Used to remember state between requests on the same stub
    state = [],
    % Counts number of requests to certain endpoint
    counter = 0
}).

