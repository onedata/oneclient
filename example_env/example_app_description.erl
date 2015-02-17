%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This is an example app description used by appmock.
%%% @end
%%%-------------------------------------------------------------------
-module(example_app_description).
-behaviour(mock_app_description_behaviour).

-include_lib("appmock/include/appmock.hrl").
-include_lib("ctool/include/logging.hrl").

-export([response_mocks/0]).

% This function should return a list of mappings {Port, Path} -> {Response}.
% If a request is performed on certain port and certain path, the response will be returned.
response_mocks() -> [
    % First type of response can be static binary. It is returned every time the endpoint is requested.
    % #mock_resp has default values for code, content_type and headers. They can be easily overriden,
    % but in most cases it's enough to specify just the 'body' field.
    % Path can be any binary compatible with cowboy's router syntax:
    % http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing/ - see matching paths.
    #mock_resp_mapping{port = 8080, path = <<"/test1/[:binding]">>, response = #mock_resp{code = 206, content_type = <<"text/plain">>,
        headers = [{<<"a">>, <<"b">>}, {<<"c">>, <<"d">>}], body = <<"this is test1 endpoint">>}},

    % Second type of response can be a list of static responses. They are returned in order of the list.
    % If the end of the list is reached, it starts from the beggining again.
    #mock_resp_mapping{port = 8080, path = <<"/test2">>, response = [
        #mock_resp{body = <<"lorem ipsum">>},
        #mock_resp{body = <<"dolor sit amet,">>},
        #mock_resp{body = <<"consectetur adipiscing elit.">>}
    ]},

    % Third type are dynamically generated responses. To define such a response, a two argument function must
    % be provided. Args are Req (cowboy req record) and State (custom state of the mapping, passed between requests).
    % Returned value is a tuple containing #mock_resp record and new state.
    #mock_resp_mapping{port = 9090, path = <<"/test_with_state">>,
        response = fun(_Req, State) ->
            R = #mock_resp{body = <<"Counter: ", (integer_to_binary(State))/binary>>},
            {R, State + 1}
        end,
        initial_state = 0},

    % If types inside the mapping are not obided by, the appmock will return a 500 Internal server error response.
    #mock_resp_mapping{port = 8080, path = <<"/test3">>, response = some_rubbish_that_will_cause_appmock_to_crash}

    % There is a cowboy_req facade module for convenience, called req.
    % It contains most useful functions to get information about incoming requests.
    % They can be used inside response functions.
    #mock_resp_mapping{port = 443, path = <<"/[:binding/[...]]">>,
        response = fun(Req, _State) ->
            Headers = req:headers(Req),
            ContentType = req:header(<<"content-type">>, Req),
            Host = req:host(Req),
            Peer = req:peer(Req),
            Path = req:path(Req),
            Binding = req:binding(binding, Req),
            Body = req:body(Req),
            PostParams = req:post_params(Req),
            ResponseBody = gui_str:format_bin(
                "Your request contained:~n" ++
                    "Host:        ~s~n" ++
                    "Path:        ~s~n" ++
                    "Binding:     ~p~n" ++
                    "Peer:        ~p~n" ++
                    "ContentType: ~p~n" ++
                    "Headers:     ~p~n" ++
                    "Body:        ~p~n" ++
                    "PostParams:  ~p~n",
                [Host, Path, Binding, Peer, ContentType, Headers, Body, PostParams]),
            {#mock_resp{body = ResponseBody, content_type = <<"text/plain">>}, whatever}
        end,
        initial_state = whatever}
].