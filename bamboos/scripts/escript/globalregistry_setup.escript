#!/usr/bin/env escript

%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This script sets globalregistry component up by initializing
%% database with custom configuration.
%% @end
%% ===================================================================
-module(globalregistry_setup).
-mode(compile).

-include_lib("public_key/include/public_key.hrl").

%% exit codes
-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).

%% globalregistry nodes cookie
-define(COOKIE, globalregistry).

%% globalregistry application name
-define(GR_APP, "globalregistry").

%% globalregistry node
-define(GR_NODE, globalregistry_node).

%% error logs filename
-define(LOGS_FILE, "globalregistry_setup.logs").

%% authorization expiration time in seconds
-define(AUTH_CODE_EXPIRATION_SECS, 3600).

%% database records
-record(oauth_account, {
    provider_id = undefined,
    user_id = <<"">>,
    login = <<"">>,
    name = <<"">>,
    email_list = []
}).

-record(user, {
    name = <<"">>,
    alias = "",
    email_list = [],
    connected_accounts = [],
    spaces = [],
    default_space = undefined,
    groups = [],
    first_space_support_token = <<"">>
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
        ok = init(),
        case Args of
            ["--initialize", ConfigFile, SetupDir] ->
                {ok, Config} = file:consult(ConfigFile),
                ok = create_resource("GLOBALREGISTRY_PROVIDERS", fun create_provider/1, [Config, SetupDir]),
                ok = create_resource("GLOBALREGISTRY_USERS", fun create_user/1, [Config]),
                ok = create_resource("GLOBALREGISTRY_GROUPS", fun create_user_group/1, [Config]),
                ok = create_resource("GLOBALREGISTRY_SPACES", fun create_space/1, [Config]);
            ["--get_client_auth_code", UserId] ->
                ok = get_client_auth_code(erlang:list_to_binary(UserId));
            ["--get_provider_auth_code", ProviderId, UserId] ->
                ok = get_provider_auth_code(erlang:list_to_binary(ProviderId), erlang:list_to_binary(UserId));
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
    put(?GR_NODE, erlang:list_to_atom(?GR_APP ++ Hostname)),
    {A, B, C} = erlang:now(),
    NodeName = "setup_node_" ++ integer_to_list(A, 32) ++ integer_to_list(B, 32) ++ integer_to_list(C, 32) ++ Hostname,
    {ok, _} = net_kernel:start([list_to_atom(NodeName), longnames]),
    true = erlang:set_cookie(node(), ?COOKIE),
    ok.


%% create_resource/3
%% ====================================================================
%% @doc Creates resource.
%% @end
-spec create_resource(Key :: binary(), Function :: function(), Args :: [term()]) -> ok.
%% ====================================================================
create_resource(Key, Function, [Config | Args]) ->
    lists:foreach(fun(ResourceConfig) ->
        ok = Function([ResourceConfig | Args])
    end, proplists:get_value(Key, Config, [])),
    ok.


%% create_provider/1
%% ====================================================================
%% @doc Creates provider.
%% @end
-spec create_provider([Args :: term()]) -> ok.
%% ====================================================================
create_provider([ProviderConfig, SetupDir]) ->
    Id = get_value("ID", ProviderConfig, <<"">>),
    Name = get_value("NAME", ProviderConfig, Id),
    URLs = get_value("URLS", ProviderConfig, []),
    RedirectionPoint = get_value("REDIRECTION_POINT", ProviderConfig, <<"">>),
    KeyFile = get_value("KEY", ProviderConfig),
    CertFile = get_value("CERT", ProviderConfig),
    LocalKeyFile = filename:join([SetupDir, <<"grpkey.pem">>]),
    LocalCSRFile = filename:join([SetupDir, <<"grpcsr.pem">>]),
    LocalCertFile = filename:join([SetupDir, <<"grpcert.pem">>]),

    case get_value("GENERATE_KEY_AND_CERT", ProviderConfig) of
        <<"yes">> ->
            os:cmd(erlang:binary_to_list(<<"openssl genrsa -out ", LocalKeyFile/binary, " 2048">>)),
            os:cmd(erlang:binary_to_list(<<"openssl req -new -batch -key ", LocalKeyFile/binary, " -out ", LocalCSRFile/binary>>)),
            {ok, CSR} = file:read_file(LocalCSRFile),

            ok = mock(dao_helper, gen_uuid, 0, erlang:binary_to_list(Id)),
            {ok, Id, Cert} = rpc_call(provider_logic, create, [Name, URLs, RedirectionPoint, CSR]),
            ok = unmock(dao_helper),

            ok = file:write_file(LocalCertFile, Cert),
            "0" = os:cmd(erlang:binary_to_list(<<"scp ", LocalKeyFile/binary, " ", KeyFile/binary, " >/dev/null 2>&1 ; echo -n $?">>)),
            "0" = os:cmd(erlang:binary_to_list(<<"scp ", LocalCertFile/binary, " ", CertFile/binary, " >/dev/null 2>&1 ; echo -n $?">>));
        _ ->
            "0" = os:cmd(erlang:binary_to_list(<<"scp ", KeyFile/binary, " ", LocalKeyFile/binary, " >/dev/null 2>&1 ; echo -n $?">>)),
            "0" = os:cmd(erlang:binary_to_list(<<"scp ", CertFile/binary, " ", LocalCertFile/binary, " >/dev/null 2>&1 ; echo -n $?">>)),
            {ok, Cert} = file:read_file(LocalCertFile),
            [{'Certificate', CertDer, not_encrypted}] = public_key:pem_decode(Cert),
            #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{serialNumber = Serial}} = public_key:pkix_decode_cert(CertDer, otp),

            ok = mock(dao_helper, gen_uuid, 0, erlang:binary_to_list(Id)),
            ok = mock(grpca, sign_provider_req, 2, {ok, Cert, Serial}),
            {ok, Id, Cert} = rpc_call(provider_logic, create, [Name, URLs, RedirectionPoint, csr]),
            ok = unmock(grpca),
            ok = unmock(dao_helper)
    end,

    ok.


%% create_user/2
%% ====================================================================
%% @doc Creates user.
%% @end
-spec create_user([UserConfig :: [{Key :: binary(), Values :: term()}]]) -> ok.
%% ====================================================================
create_user([UserConfig]) ->
    Id = get_value("ID", UserConfig, <<"">>),
    Name = get_value("NAME", UserConfig, <<"">>),
    Alias = get_value("ALIAS", UserConfig, ""),
    Emails = get_value("EMAILS", UserConfig, []),
    DefaultSpace = <<Id/binary, "_default_space">>,
    DefaultSpaceProvider = get_value("DEFAULT_SPACE_PROVIDER", UserConfig),

    ConnectedAccounts = lists:map(fun(ConnectedAccount) ->
        #oauth_account{
            provider_id = erlang:binary_to_atom(get_value("PROVIDER_ID", ConnectedAccount, <<"">>), latin1),
            user_id = get_value("USER_ID", ConnectedAccount, <<"">>),
            login = get_value("LOGIN", ConnectedAccount, <<"">>),
            name = Name,
            email_list = Emails
        }
    end, proplists:get_value("CONNECTED_ACCOUNTS", UserConfig, [])),

    User = #user{
        name = Name,
        alias = Alias,
        email_list = Emails,
        connected_accounts = ConnectedAccounts
    },

    ok = mock(dao_helper, gen_uuid, 0, erlang:binary_to_list(Id)),
    {ok, Id} = rpc_call(user_logic, create, [User]),
    ok = unmock(dao_helper),

    ok = mock(dao_helper, gen_uuid, 0, erlang:binary_to_list(DefaultSpace)),
    {ok, DefaultSpace} = rpc_call(space_logic, create, [{user, Id}, DefaultSpace]),
    ok = unmock(dao_helper),

    true = rpc_call(user_logic, set_default_space, [Id, DefaultSpace]),
    {ok, Token} = rpc_call(token_logic, create, [space_support_token, {space, DefaultSpace}]),
    ok = rpc_call(user_logic, modify, [Id, [{first_space_support_token, Token}]]),

    case DefaultSpaceProvider of
        undefined -> ok;
        _ ->
            ok = mock(token_logic, consume, 2, {ok, {space, DefaultSpace}}),
            {ok, DefaultSpace} = rpc_call(space_logic, support, [DefaultSpaceProvider, token]),
            ok = unmock(token_logic)
    end,

    ok.


%% create_user_group/1
%% ====================================================================
%% @doc Creates user group.
%% @end
-spec create_user_group([GroupConfig :: [{Key :: binary(), Values :: term()}]]) -> ok.
%% ====================================================================
create_user_group([GroupConfig]) ->
    Id = get_value("ID", GroupConfig, <<"">>),
    Name = get_value("NAME", GroupConfig, <<"">>),
    [GroupOwner | GroupMembers] = get_value("USERS", GroupConfig, []),
    UsersPrivileges = proplists:get_value("USERS_PRIVILEGES", GroupConfig, []),

    ok = mock(dao_helper, gen_uuid, 0, erlang:binary_to_list(Id)),
    {ok, Id} = rpc_call(group_logic, create, [GroupOwner, Name]),
    ok = unmock(dao_helper),

    lists:foreach(fun(GroupMember) ->
        ok = mock(token_logic, consume, 2, {ok, {group, Id}}),
        {ok, Id} = rpc_call(group_logic, join, [GroupMember, token]),
        ok = unmock(token_logic)
    end, GroupMembers),

    lists:foreach(fun(UserPrivileges) ->
        User = get_value("ID", UserPrivileges),
        Role = get_value("ROLE", UserPrivileges),
        Privileges = get_privileges(<<"group">>, Role),
        ok = rpc_call(group_logic, set_privileges, [Id, User, Privileges])
    end, UsersPrivileges),

    ok.


%% create_space/1
%% ====================================================================
%% @doc Creates space.
%% @end
-spec create_space([SpaceConfig :: [{Key :: binary(), Values :: term()}]]) -> ok.
%% ====================================================================
create_space([SpaceConfig]) ->
    Id = get_value("ID", SpaceConfig, <<"">>),
    Name = get_value("NAME", SpaceConfig, <<"">>),
    [SpaceOwner | SpaceMembers] =
        lists:map(fun(User) -> {user, User} end, get_value("USERS", SpaceConfig, [])) ++
        lists:map(fun(Group) -> {group, Group} end, get_value("GROUPS", SpaceConfig, [])),
    UsersPrivileges = lists:map(fun(UserPrivileges) ->
        User = get_value("ID", UserPrivileges),
        Role = get_value("ROLE", UserPrivileges),
        Privileges = get_privileges(<<"space">>, Role),
        {{user, User}, Privileges}
    end, proplists:get_value("USERS_PRIVILEGES", SpaceConfig, [])),
    GroupsPrivileges = lists:map(fun(GroupPrivileges) ->
        Group = get_value("ID", GroupPrivileges),
        Role = get_value("ROLE", GroupPrivileges),
        Privileges = get_privileges(<<"space">>, Role),
        {{group, Group}, Privileges}
    end, proplists:get_value("GROUPS_PRIVILEGES", SpaceConfig, [])),
    Providers = get_value("PROVIDERS", SpaceConfig, []),

    ok = mock(dao_helper, gen_uuid, 0, erlang:binary_to_list(Id)),
    {ok, Id} = rpc_call(space_logic, create, [SpaceOwner, Name]),
    ok = unmock(dao_helper),

    lists:foreach(fun(SpaceMember) ->
        ok = mock(token_logic, consume, 2, {ok, {space, Id}}),
        {ok, Id} = rpc_call(space_logic, join, [SpaceMember, token]),
        ok = unmock(token_logic)
    end, SpaceMembers),

    lists:foreach(fun({User, Privileges}) ->
        ok = rpc_call(space_logic, set_privileges, [Id, User, Privileges])
    end, UsersPrivileges ++ GroupsPrivileges),

    lists:foreach(fun(Provider) ->
        ok = mock(token_logic, consume, 2, {ok, {space, Id}}),
        {ok, Id} = rpc_call(space_logic, support, [Provider, token]),
        ok = unmock(token_logic)
    end, Providers),

    ok.


%% get_client_auth_code/1
%% ====================================================================
%% @doc Returns client's authorization code.
%% @end
-spec get_client_auth_code(UserId :: binary()) -> ok.
%% ====================================================================
get_client_auth_code(UserId) ->
    <<_/binary>> = AuthCode = rpc_call(auth_logic, gen_auth_code, [UserId]),
    io:format("~s", [erlang:binary_to_list(AuthCode)]),
    ok.


%% get_provider_auth_code/2
%% ====================================================================
%% @doc Returns provider's authorization code.
%% @end
-spec get_provider_auth_code(ProviderId :: binary(), UserId :: binary()) -> ok.
%% ====================================================================
get_provider_auth_code(ProviderId, UserId) ->
    ok = mock(utils, time, 0, 1000000000000),
    {<<_/binary>>, <<_/binary>> = RedirectionUri} = rpc_call(auth_logic, get_redirection_uri, [UserId, ProviderId]),
    ok = unmock(utils),

    [_, AuthCode] = binary:split(RedirectionUri, <<"?code=">>, [global]),
    io:format("~s", [erlang:binary_to_list(AuthCode)]),
    ok.


%% get_privileges/2
%% ====================================================================
%% @doc Returns privileges for given resource and role.
%% Resource :: <<"space">> | <<"group">>
%% Role :: <<"user">> | <<"manager">> | <<"admin">>
%% @end
-spec get_privileges(Resource :: binary(), Role :: binary()) -> [atom()].
%% ====================================================================
get_privileges(Resource, Role) ->
    Function = erlang:binary_to_atom(<<Resource/binary, "_", Role/binary>>, latin1),
    [_ | _] = rpc_call(privileges, Function, []).


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

rpc_call(Module, Function, Args) ->
    rpc:call(get(?GR_NODE), Module, Function, Args).


%% mock/3
%% ====================================================================
%% @doc Mocks function from given module on remote node.
%% @end
-spec mock(Module :: module(), Method :: atom(), Arrity :: non_neg_integer(), Value :: term()) -> ok.
%% ====================================================================
mock(Module, Method, Arity, Value) ->
    ok = rpc_call(meck, new, [Module, [passthrough, non_strict, unstick, no_link]]),
    ok = rpc_call(meck, expect, [Module, Method, Arity, Value]),
    ok.


%% unmock/1
%% ====================================================================
%% @doc Unmocks module on remote node.
%% @end
-spec unmock(Module :: module()) -> ok.
%% ====================================================================
unmock(Module) ->
    true = rpc_call(meck, validate, [Module]),
    ok = rpc_call(meck, unload, [Module]),
    {module, Module} = rpc_call(code, load_file, [Module]),
    ok.


%% print_usage/0
%% ====================================================================
%% @doc Prints script's usage.
%% @end
-spec print_usage() -> ok.
%% ====================================================================
print_usage() ->
    io:format("Usage: escript globalregistry_setup.escript [options]~n", []),
    io:format("Options:~n"),
    io:format("\t--initialize <config_path> <setup_dir>~n"),
    io:format("\t--get_client_auth_code <user_id>~n"),
    io:format("\t--get_provider_auth_code <provider_id> <user_id>~n").
