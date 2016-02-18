%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Functions that log messages provided by gen_dev script
%%% @end
%%%-------------------------------------------------------------------
-module(logger).
-author("Tomasz Lichon").

% logging
-define(PRINTING_WIDTH, 30).
-define(INDENT_WIDTH, 2).

%% API
-export([print_usage/0, pretty_print_entry/1, print/1, print/2, pretty_print_entry/2]).

%%%===================================================================
%%% Logging
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Prints usage of gen_dev script
%% @end
%%--------------------------------------------------------------------
-spec print_usage() -> ok.
print_usage() ->
    print("Usage: escript gen_dev.escript [GEN_DEV_ARGS_PATH]").

%%--------------------------------------------------------------------
%% @doc
%% pretty prints given key value entry
%% @end
%%--------------------------------------------------------------------
-spec pretty_print_entry({Key :: atom(), Value :: term()}) -> ok.
pretty_print_entry({Key, Value}) ->
    print("~*s~p", [-?PRINTING_WIDTH, atom_to_list(Key), Value]).

%%--------------------------------------------------------------------
%% @doc
%% pretty prints given key value entry
%% @end
%%--------------------------------------------------------------------
-spec pretty_print_entry({Key :: atom(), Value :: term()}, IndentDepth :: non_neg_integer()) -> ok.
pretty_print_entry({Key, Value}, IndentDepth) ->
    Width = IndentDepth * ?INDENT_WIDTH,
    print("~*s~*s~p", [Width, "", -(?PRINTING_WIDTH - Width), atom_to_list(Key), Value]).

%%--------------------------------------------------------------------
%% @doc
%% Print message
%% @end
%%--------------------------------------------------------------------
-spec print(Msg :: string()) -> ok.
print(Msg) ->
    print(Msg, []).

%%--------------------------------------------------------------------
%% @doc
%% Print message with args
%% @end
%%--------------------------------------------------------------------
-spec print(Msg :: string(), Args :: list()) -> ok.
print(Msg, Args) ->
    io:format(user, Msg ++ "~n", Args),
    Msg.

%%%===================================================================
%%% Internal functions
%%%===================================================================