%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module provides commonly used methods in all integration tests 
%%       (mainly in clusters' setup/teardown methods)         
%% @end
%% ===================================================================
-module(test_common).

-include("test_common.hrl").

-define(INFO(X, Y), io:format(X ++ "~n", Y)).

-export([wipe_db/1, register_user/1, setup/2, teardown/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CLUSTER SIDE METHODS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Wipes given databases. You should provide either list of db names or 'all' atom
%% This method is called by default in all test_suite:setup/1 (wipe_db(all))
wipe_db(all) ->
    wipe_db(["users", "files", "file_descriptors", "system_data"]);
wipe_db([DB | Rest]) ->
    dao_helper:delete_db(DB),
    wipe_db(Rest);
wipe_db([]) ->
    timer:sleep(1000), %% BigCouch needs some time before recreating databases
    dao:set_db().


%% Registers test user based on given cert file (path should be relative to common_files dir)
%% This method is called by default in all TEST:setup/1 (register_user("peer.pem"))
register_user(PEMFile) ->
    {ok, PemBin} = file:read_file(?COMMON_FILE(PEMFile)),
    Cert = public_key:pem_decode(PemBin),
    [Leaf | Chain] = [public_key:pkix_decode_cert(DerCert, otp) || {'Certificate', DerCert, _} <- Cert],
    {ok, EEC} = gsi_handler:find_eec_cert(Leaf, Chain, gsi_handler:is_proxy_certificate(Leaf)), 
    {rdnSequence, Rdn} = gsi_handler:proxy_subject(EEC),
    {ok, DnString} = user_logic:rdn_sequence_to_dn_string(Rdn),
    user_logic:create_user("veilfstestuser", "Test Name", [], "test@test.com", [DnString]).

%% Setup runs on cluster node !
setup(worker, TestName) ->
    setup_test_specific(worker, TestName);
setup(ccm, TestName) ->
    wait_for_cluster_init(),

    {ListStatus, StorageList} = dao_lib:apply(dao_vfs, list_storage, [], 1),
    case ListStatus of
        ok -> lists:foreach(fun(VeilDoc) -> dao_lib:apply(dao_vfs, remove_storage, [{uuid, element(2,VeilDoc)}], 1) end, StorageList);
        _ -> {error,storage_listing_error}
    end,
    setup_test_specific(ccm, TestName).

setup_test_specific(NodeType, TestName) ->
    %% Run test specific setup method
    R =
        try apply(list_to_atom(TestName), setup, [NodeType]) of
            Res2 -> Res2
        catch
            Type2:Error2 -> {Type2, Error2, erlang:get_stacktrace()}
        end,
    ?INFO("Setup {~p, ~p}: ~p", [NodeType, TestName, R]).


%% Teardown runs on cluster node !
teardown(NodeType, TestName, CTX) ->
    ?INFO("TearDown: ~p:~p (CTX: ~p)", [NodeType, TestName, CTX]),

    %% Run test specific teardown method
    try apply(list_to_atom(TestName), teardown, [NodeType, CTX]) of
        Res1 -> Res1
    catch
        Type1:Error1 -> {Type1, Error1, erlang:get_stacktrace()}
    end.