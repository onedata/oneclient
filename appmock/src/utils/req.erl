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
    maps:to_list(cowboy_req:headers(Req)).


%%--------------------------------------------------------------------
%% @doc
%% Returns request header by name.
%% @end
%%--------------------------------------------------------------------
-spec header(Header :: binary(), Req :: cowboy_req:req()) -> binary() | undefined.
header(Header, Req) ->
    cowboy_req:header(Header, Req).


%%--------------------------------------------------------------------
%% @doc
%% Returns host i. e. hostname that was requested by a client.
%% @end
%%--------------------------------------------------------------------
-spec host(Req :: cowboy_req:req()) -> binary() | undefined.
host(Req) ->
    cowboy_req:host(Req).


%%--------------------------------------------------------------------
%% @doc
%% Returns host i. e. hostname that was requested by a client.
%% @end
%%---------------------------------------------------------------------spec header(Header :: binary(), Req :: cowboy_req:req()) -> [{Key :: binary(), Value :: binary()}].
-spec peer(Req :: cowboy_req:req()) -> {Ip :: inet:ip_address(), Port :: inet:port_number()}.
peer(Req) ->
    cowboy_req:peer(Req).


%%--------------------------------------------------------------------
%% @doc
%% Returns requested path.
%% @end
%%--------------------------------------------------------------------
-spec path(Req :: cowboy_req:req()) -> binary().
path(Req) ->
    cowboy_req:path(Req).


%%--------------------------------------------------------------------
%% @doc
%% Returns a binding matched in path by cowboy router.
%% @end
%%--------------------------------------------------------------------
-spec binding(Binding :: atom(), Req :: cowboy_req:req()) -> binary() | undefined.
binding(Binding, Req) ->
    cowboy_req:binding(Binding, Req).
