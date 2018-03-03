%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a facade to cowboy_req module for easy access to request parameters.
%%% @end
%%%-------------------------------------------------------------------
-module(req).
-author("Lukasz Opiola").

%% API
-export([headers/1, header/2, host/1, peer/1]).
-export([path/1, binding/2]).
-export([body/1, post_params/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns request headers.
%% @end
%%--------------------------------------------------------------------
-spec headers(Req :: cowboy_req:req()) -> [{Key :: binary(), Value :: binary()}].
headers(Req) ->
    {Headers, _} = cowboy_req:headers(Req),
    Headers.


%%--------------------------------------------------------------------
%% @doc
%% Returns request header by name.
%% @end
%%--------------------------------------------------------------------
-spec header(Header :: binary(), Req :: cowboy_req:req()) -> [{Key :: binary(), Value :: binary()}] | undefined.
header(Header, Req) ->
    proplists:get_value(Header, headers(Req), undefined).


%%--------------------------------------------------------------------
%% @doc
%% Returns host i. e. hostname that was requested by a client.
%% @end
%%--------------------------------------------------------------------
-spec host(Req :: cowboy_req:req()) -> binary() | undefined.
host(Req) ->
    header(<<"host">>, Req).


%%--------------------------------------------------------------------
%% @doc
%% Returns host i. e. hostname that was requested by a client.
%% @end
%%---------------------------------------------------------------------spec header(Header :: binary(), Req :: cowboy_req:req()) -> [{Key :: binary(), Value :: binary()}].
-spec peer(Req :: cowboy_req:req()) -> {Ip :: {integer(), integer(), integer(), integer()}, Port :: integer()}.
peer(Req) ->
    {{Ip, Port}, Req} = cowboy_req:peer(Req),
    {Ip, Port}.


%%--------------------------------------------------------------------
%% @doc
%% Returns requested path.
%% @end
%%--------------------------------------------------------------------
-spec path(Req :: cowboy_req:req()) -> binary().
path(Req) ->
    {Path, _} = cowboy_req:path(Req),
    Path.


%%--------------------------------------------------------------------
%% @doc
%% Returns a binding matched in path by cowboy router.
%% @end
%%--------------------------------------------------------------------
-spec binding(Binding :: atom(), Req :: cowboy_req:req()) -> binary() | undefined.
binding(Binding, Req) ->
    {Match, _} = cowboy_req:binding(Binding, Req),
    Match.


%%--------------------------------------------------------------------
%% @doc
%% Returns request body.
%% @end
%%--------------------------------------------------------------------
-spec body(Req :: cowboy_req:req()) -> binary().
body(Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
    Body.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves all form parameters sent in POST request.
%% @end
%%--------------------------------------------------------------------
-spec post_params(Req :: cowboy_req:req()) -> Params :: [{Key :: binary(), Value :: binary()}].
post_params(Req) ->
    {ok, Params, _} = cowboy_req:body_qs(Req),
    Params.
