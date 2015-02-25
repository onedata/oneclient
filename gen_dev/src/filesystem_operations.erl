%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Functions that wrap common filesystem operations
%%% @end
%%%-------------------------------------------------------------------
-module(filesystem_operations).
-author("Tomasz Lichon").

%% API
-export([make_dir/1, remove_dir/1, copy_dir/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% wraps system 'mkdir -p'
%% @end
%%--------------------------------------------------------------------
-spec make_dir(Path :: string()) -> ok | no_return().
make_dir(Path) ->
    case os:cmd("mkdir -p '" ++ Path ++ "'") of
        [] -> ok;
        Err -> throw(Err)
    end.

%%--------------------------------------------------------------------
%% @doc
%% wraps system 'rm -rf'
%% @end
%%--------------------------------------------------------------------
-spec remove_dir(Path :: string()) -> ok | no_return().
remove_dir(Path) ->
    case os:cmd("rm -rf '" ++ Path ++ "'") of
        [] -> ok;
        Err -> throw(Err)
    end.

%%--------------------------------------------------------------------
%% @doc
%% wraps system 'cp -R'
%% @end
%%--------------------------------------------------------------------
-spec copy_dir(From :: string(), To :: string()) -> ok | no_return().
copy_dir(From, To) ->
    case os:cmd("cp -RTf '" ++ From ++ "' '" ++ To ++ "'") of
        [] -> ok;
        Err -> throw(Err)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================