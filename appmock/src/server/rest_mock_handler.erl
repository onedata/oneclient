%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a cowboy handler and processes requests to mocked REST endpoints.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_mock_handler).
-behaviour(cowboy_handler).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
-include("appmock.hrl").

%% Cowboy API
-export([init/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Cowboy callback, called to initialize the state of the handler.
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), [ETSKey]) -> {ok, cowboy_req:req(), [ETSKey]} when ETSKey :: {Port :: integer(), Path :: binary()}.
init(Req, [ETSKey] = State) ->
    Req2 = cowboy_req:set_resp_header(<<"connection">>, <<"close">>, Req),

    NewReq = try
        {ok, {Code, Headers, Body}} = rest_mock_server:produce_response(Req2, ETSKey),
        Req3 = cowboy_req:set_resp_body(Body, Req2),
        Req4 = lists:foldl(
            fun({HKey, HValue}, CurrReq) ->
                cowboy_req:set_resp_header(HKey, HValue, CurrReq)
            end, Req3, Headers),
        cowboy_req:reply(Code, Req4)
    catch T:M ->
        {Port, Path} = ETSKey,
        Stacktrace = erlang:get_stacktrace(),
        ?error("Error in ~p. Path: ~p. Port: ~p. ~p:~p.~nStacktrace: ~p",
            [?MODULE, Path, Port, T, M, Stacktrace]),
        Error = str_utils:format_bin("500 Internal server error - make sure that your description file does not " ++
        "contain errors.~n-----------------~nType:       ~p~nMessage:    ~p~nStacktrace: ~p", [T, M, Stacktrace]),
        ErrorReq2 = cowboy_req:set_resp_body(Error, Req2),
        ErrorReq3 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, ErrorReq2),
        cowboy_req:reply(500, ErrorReq3)
    end,
    {ok, NewReq, State}.
