#!/usr/bin/env escript

%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This script sets oneprovider and corresponding onepanel 
%% components up by initializing their databases with custom configuration.
%% @end
%% ===================================================================
-module(oneprovider_setup).

%% exit codes
-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).

%% globalregistry nodes cookie
-define(COOKIE, oneprovider_node).

%% oneprovider's onepanel application name
-define(ONEPANEL_APP, "onepanel").

%% oneprovider's onepanel node
-define(ONEPANEL_NODE, onepanel_node).

%% oneprovider's worker application name
-define(WORKER_APP, "worker").

%% oneprovider's worker node
-define(WORKER_NODE, worker_node).

%% error logs filename
-define(LOGS_FILE, "oneprovider_setup.logs").

%% openID records
-record(id_token, {
    iss = <<"">>,
    sub = <<"">>,
    aud = <<"">>,
    name = <<"">>,
    logins = [],
    emails = [],
    exp = <<"">>,
    iat = <<"">>
}).

-record(token_response, {
    access_token = <<"">>,
    token_type = <<"">>,
    expires_in = 0,
    refresh_token = <<"">>,
    scope = <<"">>,
    id_token :: #id_token{}
}).

%% onepanel records
-record(provider_details, {
    id = <<"">>,
    name = <<"">>,
    redirection_point = <<"">>,
    urls = []
}).

%% ====================================================================
%% API
%% ====================================================================

-export([main/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% main/1
%% ====================================================================
%% @doc Script entry function.
%% @end
-spec main(Args :: [string()]) -> no_return().
%% ====================================================================
main(Args) ->
    try
        case Args of
            ["--initialize", ConfigFile, SetupDir] ->
                init(),
                {ok, Config} = file:consult(ConfigFile),
                ok = source_provider_key_and_cert(Config),
                ok = create_provider(Config),
                ok = create_users(Config, SetupDir);
            _ ->
                print_usage()
        end,
        halt(?EXIT_SUCCESS)
    catch
        Error:Reason ->
            Log = io_lib:fwrite("Error: ~p~nReason: ~p~nStacktrace: ~p~n", [Error, Reason, erlang:get_stacktrace()]),
            file:write_file(?LOGS_FILE, Log),
            io:format("An error occured. See ~s for more information.~n", [?LOGS_FILE]),
            halt(?EXIT_FAILURE)
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% init/0
%% ====================================================================
%% @doc Sets up net kernel and establishes connection to oneprovider node.
%% @end
-spec init() -> ok.
%% ====================================================================
init() ->
    Hostname = "@" ++ os:cmd("hostname -f") -- "\n",
    put(?ONEPANEL_NODE, erlang:list_to_atom(?ONEPANEL_APP ++ Hostname)),
    put(?WORKER_NODE, erlang:list_to_atom(?WORKER_APP ++ Hostname)),
    {A, B, C} = erlang:now(),
    NodeName = "setup_node_" ++ integer_to_list(A, 32) ++ integer_to_list(B, 32) ++ integer_to_list(C, 32) ++ Hostname,
    {ok, _} = net_kernel:start([list_to_atom(NodeName), longnames]),
    true = erlang:set_cookie(node(), ?COOKIE).


%% create_provider/1
%% ====================================================================
%% @doc Creates oneprovider in onepanel database.
%% @end
-spec create_provider(Config :: [{Key :: binary(), Values :: term()}]) -> ok.
%% ====================================================================
create_provider(Config) ->
    case get_value("ONEPROVIDER_ID", Config) of
        undefined ->
            ok;
        ProviderId ->
            #provider_details{} = ProviderDetails = lists:foldl(fun
                (ProviderConfig, undefined) ->
                    case get_value("ID", ProviderConfig) of
                        ProviderId ->
                            #provider_details{
                                id = ProviderId,
                                name = get_value("NAME", ProviderConfig, <<"">>),
                                urls = get_value("URLS", ProviderConfig, []),
                                redirection_point = get_value("REDIRECTION_POINT", ProviderConfig, <<"">>)
                            };
                        _ ->
                            undefined
                    end;
                (_, Details) -> Details
            end, undefined, proplists:get_value("GLOBALREGISTRY_PROVIDERS", Config, [])),
            rpc:call(get(?ONEPANEL_NODE), dao, save_record, [providers, ProviderDetails]),
            ok
    end.


%% create_users/1
%% ====================================================================
%% @doc Creates users in oneprovider's database.
%% @end
-spec create_users(Config :: [{Key :: binary(), Values :: term()}], SetupDir :: string()) -> ok.
%% ====================================================================
create_users(Config, SetupDir) ->
    ProviderId = get_value("ONEPROVIDER_ID", Config),
    UserIds = get_value("ONECLIENT_USERS", Config, []),
    UserCerts = get_value("ONECLIENT_CERTS", Config, []),
    DNStrings = lists:map(fun(RemoteCertFile) ->
        CertFile = filename:join([SetupDir, <<"usercert.pem">>]),
        "0" = os:cmd(erlang:binary_to_list(<<"scp ", RemoteCertFile/binary, " ", CertFile/binary, " >/dev/null 2>&1 ; echo -n $?">>)),
        {ok, Cert} = file:read_file(CertFile),
        CertDecoded = public_key:pem_decode(Cert),
        [Leaf | Chain] = [public_key:pkix_decode_cert(DerCert, otp) || {'Certificate', DerCert, _} <- CertDecoded],
        IsProxyCert = rpc:call(get(?WORKER_NODE), gsi_handler, is_proxy_certificate, [Leaf]),
        {ok, EEC} = rpc:call(get(?WORKER_NODE), gsi_handler, find_eec_cert, [Leaf, Chain, IsProxyCert]),
        {rdnSequence, Rdn} = rpc:call(get(?WORKER_NODE), gsi_handler, proxy_subject, [EEC]),
        {ok, DnString} = rpc:call(get(?WORKER_NODE), user_logic, rdn_sequence_to_dn_string, [Rdn]),
        DnString
    end, UserCerts),
    DnStringByUserId = lists:zip(UserIds, DNStrings),
    UniqueUserIds = lists:usort(UserIds),
    lists:foreach(fun(UserId) ->
        {ok, AuthCode} = get_provider_auth_code(Config, SetupDir, ProviderId, UserId),
        UserDNStrings = lists:usort(proplists:get_all_values(UserId, DnStringByUserId)),
        {ok, #token_response{
            access_token = AccessToken,
            refresh_token = RefreshToken,
            expires_in = ExpiresIn,
            id_token = #id_token{
                sub = GRUID,
                name = Name,
                logins = Logins,
                emails = Emails
            }
        }} = rpc:call(get(?WORKER_NODE), gr_openid, get_token_response, [
            provider,
            [{<<"code">>, AuthCode}, {<<"grant_type">>, <<"authorization_code">>}]
        ]),
        UserDetails = [
            {global_id, unicode:characters_to_list(GRUID)},
            {logins, Logins},
            {name, unicode:characters_to_list(Name)},
            {teams, []},
            {emails, lists:map(fun(Email) -> unicode:characters_to_list(Email) end, Emails)},
            {dn_list, UserDNStrings}
        ],
        ExpirationTime = rpc:call(get(?WORKER_NODE), utils, time, []) + ExpiresIn,
        {[_ | _], _} = rpc:call(get(?WORKER_NODE), user_logic, sign_in, [UserDetails, AccessToken, RefreshToken, ExpirationTime])
    end, UniqueUserIds),
    ok.


%% source_provider_key_and_cert/1
%% ====================================================================
%% @doc Copies oneprovider's private key and certificate, signed by globalregistry,
%% from given source to certificate directory on all workers.
%% @end
-spec source_provider_key_and_cert(Config :: [{Key :: binary(), Values :: term()}]) -> ok.
%% ====================================================================
source_provider_key_and_cert(Config) ->
    case get_value("ONEPROVIDER_ID", Config) of
        undefined ->
            ok;
        ProviderId ->
            {<<_/binary>> = SourceKeyFile, <<_/binary>> = SourceCertFile} = lists:foldl(fun
                (ProviderConfig, {undefined, undefined}) ->
                    case get_value("ID", ProviderConfig, <<"">>) of
                        ProviderId ->
                            {
                                get_value("KEY", ProviderConfig, <<"">>),
                                get_value("CERT", ProviderConfig, <<"">>)
                            };
                        _ ->
                            {undefined, undefined}
                    end;
                (_, SourceFiles) ->
                    SourceFiles
            end, {undefined, undefined}, proplists:get_value("GLOBALREGISTRY_PROVIDERS", Config, [])),
            lists:foreach(fun(Node) ->
                lists:foreach(fun(Type) ->
                    "0" = os:cmd(erlang:binary_to_list(<<"scp ", SourceKeyFile/binary, " ", Node/binary,
                    ":/opt/oneprovider/nodes/", Type/binary, "/certs/grpkey.pem >/dev/null 2>&1 ; echo -n $?">>)),
                    "0" = os:cmd(erlang:binary_to_list(<<"scp ", SourceCertFile/binary, " ", Node/binary,
                    ":/opt/oneprovider/nodes/", Type/binary, "/certs/grpcert.pem >/dev/null 2>&1 ; echo -n $?">>))
                end, [<<"worker">>, <<"onepanel">>])
            end, get_value("ONEPROVIDER_NODES", Config, [])),
            ok
    end.


%% get_provider_auth_code/4
%% ====================================================================
%% @doc Generates and returns authorization code.
%% @end
-spec get_provider_auth_code(Config :: [{Key :: binary(), Values :: term()}], SetupDir :: string(), ProviderId :: binary(), UserId :: binary()) -> {ok, AuthCode :: binary()}.
%% ====================================================================
get_provider_auth_code(Config, SetupDir, ProviderId, UserId) ->
    [Node | _] = get_value("GLOBALREGISTRY_NODES", Config),
    Escript = os:cmd(erlang:binary_to_list(<<"ssh ", Node/binary, " \"find /opt/globalregistry -name escript | head -1\" 2>/dev/null">>)) -- "\n",
    EscriptBin = erlang:list_to_binary(Escript),
    Script = filename:join([SetupDir, <<"globalregistry_setup.escript">>]),
    AuthCode = os:cmd(erlang:binary_to_list(<<"ssh ", Node/binary, " \"", EscriptBin/binary, " ",
    Script/binary, " --get_provider_auth_code ", ProviderId/binary, " ", UserId/binary, "\" 2>/dev/null">>)),
    {ok, erlang:list_to_binary(AuthCode)}.


%% get_value/2
%% ====================================================================
%% @equiv get_value(Key, Values, undefined)
%% @end
-spec get_value(Key :: list(), Values :: string() | [string()]) -> term().
%% ====================================================================
get_value(Key, Values) ->
    get_value(Key, Values, undefined).


%% get_value/3
%% ====================================================================
%% @doc Return value form key-value pairs for given key. If key is not
%% found among available keys, default value is returned.
%% @end
-spec get_value(Key :: list(), Values :: string() | [string()], Default :: term()) -> term().
%% ====================================================================
get_value(Key, Values, Default) ->
    case proplists:get_value(Key, Values) of
        undefined -> Default;
        [Head | _] = List when is_list(Head) ->
            lists:map(fun(Element) -> list_to_binary(Element) end, List);
        Value -> list_to_binary(Value)
    end.


%% print_usage/0
%% ====================================================================
%% @doc Prints script's usage.
%% @end
-spec print_usage() -> ok.
%% ====================================================================
print_usage() ->
    io:format("Usage: escript oneprovider_setup.escript [options]~n", []),
    io:format("Options:~n"),
    io:format("\t--initialize <config_path> <setup_dir>~n").
