#!/usr/bin/env escript
%%! -name create_storage@test_env

-export([main/1]).

main([Cookie, Node, Name, LatencyMin, LatencyMax, TimeoutProbability, Filter, Insecure]) ->
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    NodeAtom = list_to_atom(Node),

    UserCtx = safe_call(NodeAtom, helper, new_nulldevice_user_ctx, [0, 0]),
    Helper = safe_call(NodeAtom, helper, new_nulldevice_helper, [
        #{
            <<"latencyMin">> => list_to_binary(LatencyMin),
            <<"latencyMax">> => list_to_binary(LatencyMax),
            <<"timeoutProbability">> => list_to_binary(TimeoutProbability),
            <<"filter">> => list_to_binary(Filter)
        },
        UserCtx,
        list_to_atom(Insecure)
    ]),

    StorageDoc = safe_call(NodeAtom, storage, new, [list_to_binary(Name), [Helper]]),
    safe_call(NodeAtom, storage, create, [StorageDoc]).

safe_call(Node, Module, Function, Args) ->
    case rpc:call(Node, Module, Function, Args) of
        {badrpc, X} ->
            io:format(standard_error, "ERROR: in module ~p:~n {badrpc, ~p} in rpc:call(~p, ~p, ~p, ~p).~n",
                [?MODULE, X, Node, Module, Function, Args]),
            halt(42);
        {error, X} ->
            io:format(standard_error, "ERROR: in module ~p:~n {error, ~p} in rpc:call(~p, ~p, ~p, ~p).~n",
                [?MODULE, X, Node, Module, Function, Args]),
            halt(42);
        X ->
            X
    end.
