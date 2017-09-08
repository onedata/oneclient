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
-behaviour(cowboy_http_handler).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
-include("appmock.hrl").

%% Cowboy API
-export([init/3, handle/2, terminate/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Cowboy callback, called to initialize the state of the handler.
%% @end
%%--------------------------------------------------------------------
-spec init(Type :: term(), Req :: cowboy_req:req(), [ETSKey]) -> {ok, cowboy_req:req(), [ETSKey]} when ETSKey :: {Port :: integer(), Path :: binary()}.
init(_Type, Req, [ETSKey]) ->
    % This is a REST endpoint, close connection after every request.
    {ok, cowboy_req:set([{connection, close}], Req), [ETSKey]}.


%%--------------------------------------------------------------------
%% @doc
%% Cowboy callback, called to process a request.
%% Handles requests to mocked endpoints by delegating them to appmock_server.
%% @end
%%--------------------------------------------------------------------
-spec handle(Req :: cowboy_req:req(), [ETSKey]) -> {ok, cowboy_req:req(), [ETSKey]} when ETSKey :: {Port :: integer(), Path :: binary()}.
handle(Req, [ETSKey]) ->
    {ok, NewReq} =
        try
            {ok, {Code, Headers, Body}} = rest_mock_server:produce_response(Req, ETSKey),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            Req3 = lists:foldl(
                fun({HKey, HValue}, CurrReq) ->
                    cowboy_req:set_resp_header(HKey, HValue, CurrReq)
                end, Req2, Headers),
            {ok, _NewReq} = cowboy_req:reply(Code, Req3)
        catch T:M ->
            {Port, Path} = ETSKey,
            Stacktrace = erlang:get_stacktrace(),
            ?error("Error in ~p. Path: ~p. Port: ~p. ~p:~p.~nStacktrace: ~p",
                [?MODULE, Path, Port, T, M, Stacktrace]),
            Error = str_utils:format_bin("500 Internal server error - make sure that your description file does not " ++
            "contain errors.~n-----------------~nType:       ~p~nMessage:    ~p~nStacktrace: ~p", [T, M, Stacktrace]),
            ErrorReq2 = cowboy_req:set_resp_body(Error, Req),
            ErrorReq3 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, ErrorReq2),
            {ok, _ErrorReq} = cowboy_req:reply(500, ErrorReq3)
        end,
    {ok, NewReq, [ETSKey]}.


%%--------------------------------------------------------------------
%% @doc
%% Cowboy callback, called to perform cleanup after the request is handled.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), Req :: cowboy_req:req(), State :: term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.