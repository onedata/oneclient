%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% gen_dev config parsing functions
%%% @end
%%%-------------------------------------------------------------------
-module(args_parser).
-author("Tomasz Lichon").

%% API
-export([parse_config_file/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parses given json arg file to erlang proplist format
%% @end
%%--------------------------------------------------------------------
-spec parse_config_file(ArgsFile :: string()) -> list().
parse_config_file(ArgsFile) ->
    {ok, FileContent} = file:read_file(ArgsFile),
    Json = mochijson2:decode(FileContent, [{format, proplist}]),
    json_proplist_to_term(Json).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @equiv json_proplist_to_term(Json, atom)
%% @end
%%--------------------------------------------------------------------
-spec json_proplist_to_term(Json :: term()) -> term().
json_proplist_to_term(Json) ->
    json_proplist_to_term(Json, atom).

%%--------------------------------------------------------------------
%% @doc
%% Converts json in binary proplist form, to proplist with atoms and strings
%% @end
%%--------------------------------------------------------------------
-spec json_proplist_to_term(term(), DefaultConversion :: string | atom) -> term().
json_proplist_to_term([], _) ->
    [];
json_proplist_to_term([{<<"string">>, V}], _) when is_binary(V) ->
    binary_to_list(V);
json_proplist_to_term({<<"string">>, V}, _) when is_binary(V) ->
    binary_to_list(V);
json_proplist_to_term([{<<"atom">>, V}], _) when is_binary(V) ->
    binary_to_atom(V, unicode);
json_proplist_to_term({<<"atom">>, V}, _) when is_binary(V) ->
    binary_to_atom(V, unicode);
json_proplist_to_term({<<"config">>, V}, _) ->
    {config, json_proplist_to_term(V, string)};
json_proplist_to_term({<<"sys.config">>, V}, _) ->
    {'sys.config', json_proplist_to_term(V, atom)};
json_proplist_to_term({<<"vm.args">>, V}, _) ->
    {'vm.args', json_proplist_to_term(V, string)};
json_proplist_to_term({K, V}, DefaultType) ->
    {binary_to_atom(K, unicode), json_proplist_to_term(V, DefaultType)};
json_proplist_to_term([Head | Tail], DefaultType) ->
    [json_proplist_to_term(Head, DefaultType) | json_proplist_to_term(Tail, DefaultType)];
json_proplist_to_term(Binary, atom) when is_binary(Binary) ->
    binary_to_atom(Binary, unicode);
json_proplist_to_term(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
json_proplist_to_term(Other, _) ->
    Other.
