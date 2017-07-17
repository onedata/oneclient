#!/usr/bin/env escript
%%! -name create_storage@test_env

-export([main/1]).

main([Cookie, Node, Name, Hostname, Scheme, BucketName, AccessKey, SecretKey,
    Insecure]) ->

    erlang:set_cookie(node(), list_to_atom(Cookie)),
    NodeAtom = list_to_atom(Node),

    UserCtx = safe_call(NodeAtom, helper, new_s3_user_ctx, [
        list_to_binary(AccessKey),
        list_to_binary(SecretKey)
    ]),
    Helper = safe_call(NodeAtom, helper, new_s3_helper, [
        list_to_binary(Hostname),
        list_to_binary(BucketName),
        Scheme =:= "https",
        #{},
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
