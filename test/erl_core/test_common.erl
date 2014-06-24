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

-export([wipe_db/1, register_user/1]).


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
