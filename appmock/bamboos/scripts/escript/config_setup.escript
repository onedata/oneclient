#!/usr/bin/env escript

%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This script parses and sources configuration file.
%% @end
%% ===================================================================
-module(config_setup).

%% exit codes
-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).

%% all supported configuration keys
-define(KEYS, [
    "PLATFORM_CLEANUP", "ONECLIENT_USERS", "ONECLIENT_NODES", "ONECLIENT_MOUNTS", "ONECLIENT_CERTS",
    "ONECLIENT_AUTH", "ONEPROVIDER_NODES", "ONEPROVIDER_TYPES", "ONEPROVIDER_DB_NODES", "ONEPROVIDER_STORAGE_PATHS",
    "ONEPROVIDER_DEVELOPER_MODE", "GLOBALREGISTRY_NODES", "GLOBALREGISTRY_DB_NODES", "GLOBALREGISTRY_REST_CERT_DOMAIN"
]).

%% error logs filename
-define(LOGS_FILE, "config_setup.logs").

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
main([ConfigFile | Options]) ->
    try
        {ok, Config} = file:consult(ConfigFile),
        case Options of
            ["--source_all"] -> source(Config, all);
            ["--source", Key] -> source(Config, Key);
            _ -> print_usage()
        end,
        halt(?EXIT_SUCCESS)
    catch
        Error:Reason ->
            Log = io_lib:fwrite("Error: ~p~nReason: ~p~nStacktrace: ~p~n", [Error, Reason, erlang:get_stacktrace()]),
            file:write_file(?LOGS_FILE, Log),
            io:format("An error occured. See ~s for more information.~n", [?LOGS_FILE]),
            halt(?EXIT_FAILURE)
    end;

main(_) ->
    print_usage(),
    halt(?EXIT_FAILURE).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% get_value/2
%% ====================================================================
%% @doc Prints formated configuration value for given key or all
%% allowed keys.
%% @end
-spec source(Config :: [{Key :: string(), Values :: string() | [string()]}], Key :: all | string()) -> Value :: string().
%% ====================================================================
source(Config, all) ->
    Values = lists:map(fun(Key) ->
        get_value(Config, Key)
    end, ?KEYS),
    io:format("~s", [string:join(Values, " ; ")]);

source(Config, Key) ->
    true = lists:member(Key, ?KEYS),
    Value = get_value(Config, Key),
    io:format("~s", [Value]).


%% get_value/2
%% ====================================================================
%% @doc Returns formated configuration value for given key.
%% @end
-spec get_value(Config :: [{Key :: string(), Values :: string() | [string()]}], Key :: string()) -> Value :: string().
%% ====================================================================
get_value(Config, Key) ->
    case proplists:get_value(Key, Config, "") of
        [Head | _] = Values when is_list(Head) ->
            Value = string:join(Values, " ; "),
            Key ++ "=\"" ++ Value ++ "\"";
        Value when is_list(Value) ->
            Key ++ "=\"" ++ Value ++ "\""
    end.


%% print_usage/0
%% ====================================================================
%% @doc Prints script's usage.
%% @end
-spec print_usage() -> ok.
%% ====================================================================
print_usage() ->
    io:format("Usage: escript config_setup.escript <config_path> [options]~n", []),
    io:format("Options:~n"),
    io:format("\t--source <key>~n"),
    io:format("\t--source_all~n").