/**
 * @file options_test.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "options.h"
#include "oneException.h"

#include <gtest/gtest.h>
#include <gmock/gmock.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include <fstream>

using namespace one::client;
using namespace ::testing;

namespace {
const auto USER_CONFIG_PATH = boost::filesystem::unique_path();
const auto GLOBAL_CONFIG_PATH = boost::filesystem::unique_path();
constexpr auto ARG0 = "./oneclient";
constexpr auto PROVIDER_HOSTNAME_VAL = "hostname";
constexpr auto PROVIDER_PORT_CONF_VAL = "1234";
constexpr auto PROVIDER_PORT_ENV_VAL = "4321";
constexpr auto AUTHENTICATION_VAL = "token";
constexpr auto CLUSTER_PING_INTERVAL_VAL = "30";
} // namespace

class OptionsTest : public Test {
public:
    OptionsTest() {}

    ~OptionsTest()
    {
        unsetenv("NO_CHECK_CERTIFICATE");
        unsetenv("PROVIDER_PORT");
        unsetenv("PROVIDER_HOSTNAME");
        boost::filesystem::remove(USER_CONFIG_PATH);
        boost::filesystem::remove(GLOBAL_CONFIG_PATH);
    }

protected:
    Options options{GLOBAL_CONFIG_PATH};
};

TEST_F(OptionsTest, optionsShouldParseCommandLineArguments)
{
    const char *const argv[] = {ARG0, "-d", "--debug_gsi", "--proxyio",
        "--authentication", AUTHENTICATION_VAL};
    EXPECT_EQ(false, options.get_debug());
    EXPECT_EQ(false, options.get_debug_gsi());
    EXPECT_EQ(false, options.get_proxyio());
    EXPECT_NE(AUTHENTICATION_VAL, options.get_authentication());

    options.parseConfigs(6, argv);
    EXPECT_EQ(true, options.get_debug());
    EXPECT_EQ(true, options.get_debug_gsi());
    EXPECT_EQ(true, options.get_proxyio());
    EXPECT_EQ(AUTHENTICATION_VAL, options.get_authentication());
}

TEST_F(OptionsTest, optionsShouldParseHelpCommandLineArgument)
{
    const char *const argv[] = {ARG0, "--help"};
    EXPECT_EQ(false, options.get_help());
    options.parseConfigs(2, argv);
    EXPECT_EQ(true, options.get_help());
}

TEST_F(OptionsTest, optionsShouldParseVersionCommandLineArgument)
{
    const char *const argv[] = {ARG0, "--version"};
    EXPECT_EQ(false, options.get_version());
    options.parseConfigs(2, argv);
    EXPECT_EQ(true, options.get_version());
}

TEST_F(OptionsTest, shouldThrowOneExceptionOnUnknownCommandLineArgument)
{
    const char *const argv[] = {ARG0, "--unknown"};
    ASSERT_THROW(options.parseConfigs(2, argv), OneException);
}

TEST_F(OptionsTest, shouldThrowOneExceptionOnMissingCommandLineArgumentValue)
{
    const char *const argv[] = {ARG0, "--config"};
    ASSERT_THROW(options.parseConfigs(2, argv), OneException);
}

TEST_F(
    OptionsTest, shouldThrowOneExceptionOnTooManyPositionalCommandLineArguments)
{
    const char *const argv[] = {ARG0, "positional", "positional"};
    ASSERT_THROW(options.parseConfigs(3, argv), OneException);
}

TEST_F(OptionsTest,
    shouldThrowOneExceptionOnOptionSpecifiedMoreThanOnceInCommandLineArguments)
{
    const char *const argv[] = {ARG0, "--config", "val", "--config", "val2"};
    ASSERT_THROW(options.parseConfigs(5, argv), OneException);
}

TEST_F(OptionsTest, optionsShouldParseEnvironmentOptions)
{
    const char *const argv[] = {ARG0};
    setenv("NO_CHECK_CERTIFICATE", "true", 1);
    setenv("PROVIDER_PORT", PROVIDER_PORT_ENV_VAL, 1);
    setenv("PROVIDER_HOSTNAME", PROVIDER_HOSTNAME_VAL, 1);

    EXPECT_EQ(false, options.get_no_check_certificate());
    EXPECT_NE(std::stoi(PROVIDER_PORT_ENV_VAL), options.get_provider_port());
    EXPECT_NE(PROVIDER_HOSTNAME_VAL, options.get_provider_hostname());

    options.parseConfigs(1, argv);
    EXPECT_EQ(true, options.get_no_check_certificate());
    EXPECT_EQ(std::stoi(PROVIDER_PORT_ENV_VAL), options.get_provider_port());
    EXPECT_EQ(PROVIDER_HOSTNAME_VAL, options.get_provider_hostname());
}

TEST_F(OptionsTest, optionsShouldParseUserConfigurationFile)
{
    const char *const argv[] = {ARG0, "--config", USER_CONFIG_PATH.c_str()};
    boost::filesystem::ofstream userConfig(USER_CONFIG_PATH);
    userConfig << "no_check_certificate = " << true
               << "\nprovider_port = " << PROVIDER_PORT_ENV_VAL
               << "\nprovider_hostname = " << PROVIDER_HOSTNAME_VAL
               << std::endl;
    userConfig.close();

    EXPECT_EQ(false, options.get_no_check_certificate());
    EXPECT_NE(std::stoi(PROVIDER_PORT_ENV_VAL), options.get_provider_port());
    EXPECT_NE(PROVIDER_HOSTNAME_VAL, options.get_provider_hostname());

    options.parseConfigs(3, argv);
    EXPECT_EQ(true, options.get_no_check_certificate());
    EXPECT_EQ(std::stoi(PROVIDER_PORT_ENV_VAL), options.get_provider_port());
    EXPECT_EQ(PROVIDER_HOSTNAME_VAL, options.get_provider_hostname());
}

/**
* Tests whether parsing configs throws exception for user config with given
* content
* @param options
* @param fileContent
*/
void testUserConfigFileContentError(Options &options, std::string fileContent)
{
    const char *const argv[] = {ARG0, "--config", USER_CONFIG_PATH.c_str()};
    boost::filesystem::ofstream config(USER_CONFIG_PATH);
    config << fileContent << std::endl;
    config.close();

    ASSERT_THROW(options.parseConfigs(3, argv), OneException);
}

TEST_F(OptionsTest,
    shouldThrowOneExceptionWhenRestrictedOptionIsSpecifiedInUserConfig)
{
    testUserConfigFileContentError(options, "cluster_ping_interval = 30");
}

TEST_F(OptionsTest, shouldThrowOneExceptionOnUnknownOptionInUserConfig)
{
    testUserConfigFileContentError(options, "unknown = unknown");
}

TEST_F(OptionsTest, shouldThrowOneExceptionWhenFormatInUserConfigIsInvalid)
{
    testUserConfigFileContentError(options, "invalid format");
}

TEST_F(OptionsTest,
    shouldThrowOneExceptionOnOptionSpecifiedMoreThanOnceInUserConfig)
{
    testUserConfigFileContentError(
        options, "provider_hostname = name\nprovider_hostname = host");
}

TEST_F(OptionsTest, optionsShouldParseGlobalConfigurationFile)
{
    const char *const argv[] = {ARG0};
    boost::filesystem::ofstream globalConfig(GLOBAL_CONFIG_PATH);
    globalConfig << "no_check_certificate = " << true
                 << "\nprovider_port = " << PROVIDER_PORT_ENV_VAL
                 << "\nprovider_hostname = " << PROVIDER_HOSTNAME_VAL
                 << "\ncluster_ping_interval = " << CLUSTER_PING_INTERVAL_VAL
                 << std::endl;
    globalConfig.close();

    EXPECT_EQ(false, options.get_no_check_certificate());
    EXPECT_NE(std::stoi(PROVIDER_PORT_ENV_VAL), options.get_provider_port());
    EXPECT_NE(PROVIDER_HOSTNAME_VAL, options.get_provider_hostname());
    EXPECT_NE(std::stoi(CLUSTER_PING_INTERVAL_VAL),
        options.get_cluster_ping_interval());

    options.parseConfigs(1, argv);
    EXPECT_EQ(true, options.get_no_check_certificate());
    EXPECT_EQ(std::stoi(PROVIDER_PORT_ENV_VAL), options.get_provider_port());
    EXPECT_EQ(PROVIDER_HOSTNAME_VAL, options.get_provider_hostname());
    EXPECT_EQ(std::stoi(CLUSTER_PING_INTERVAL_VAL),
        options.get_cluster_ping_interval());
}

/**
* Tests whether parsing configs throws exception for global config with given
* content
* @param options
* @param fileContent
*/
void testGlobalConfigFileContentError(Options &options, std::string fileContent)
{
    const char *const argv[] = {ARG0};
    boost::filesystem::ofstream config(GLOBAL_CONFIG_PATH);
    config << fileContent << std::endl;
    config.close();

    ASSERT_THROW(options.parseConfigs(1, argv), OneException);
}

TEST_F(OptionsTest, shouldThrowOneExceptionOnUnknownOptionInGlobalConfig)
{
    testGlobalConfigFileContentError(options, "unknown = unknown");
}

TEST_F(OptionsTest, shouldThrowOneExceptionWhenFormatInGlobalConfigIsInvalid)
{
    testGlobalConfigFileContentError(options, "invalid format");
}

TEST_F(OptionsTest,
    shouldThrowOneExceptionOnOptionSpecifiedMoreThanOnceInGlobalConfig)
{
    testGlobalConfigFileContentError(
        options, "provider_hostname = name\nprovider_hostname = host");
}

TEST_F(
    OptionsTest, environmentOptionsShouldNotOverrideWhenOverridingIsNotAllowed)
{
    const char *const argv[] = {ARG0};
    setenv("PROVIDER_PORT", PROVIDER_PORT_ENV_VAL, 1);
    boost::filesystem::ofstream globalConfig(GLOBAL_CONFIG_PATH);
    globalConfig << "provider_port = " << PROVIDER_PORT_CONF_VAL
                 << "\nenable_env_option_override = false" << std::endl;
    globalConfig.close();

    EXPECT_NE(std::stoi(PROVIDER_PORT_CONF_VAL), options.get_provider_port());
    options.parseConfigs(1, argv);
    EXPECT_EQ(std::stoi(PROVIDER_PORT_CONF_VAL), options.get_provider_port());
}

TEST_F(OptionsTest, environmentOptionsShouldOverrideWhenOverridingIsAllowed)
{
    const char *const argv[] = {ARG0};
    setenv("PROVIDER_PORT", PROVIDER_PORT_ENV_VAL, 1);
    EXPECT_NE(std::stoi(PROVIDER_PORT_ENV_VAL), options.get_provider_port());
    options.parseConfigs(1, argv);
    EXPECT_EQ(std::stoi(PROVIDER_PORT_ENV_VAL), options.get_provider_port());
}

TEST_F(OptionsTest, fuseArgsShouldBeParsedAndReturnedAsFuseArgsStruct)
{
    const char *const argv[] = {ARG0, "-f", "-o", "opt", "mountpoint"};
    options.parseConfigs(5, argv);

    auto fuse_args = options.getFuseArgs();
    auto fuse_argv = std::vector<std::string>();
    for (int i = 0; i < fuse_args.argc; ++i)
        fuse_argv.push_back(std::string(fuse_args.argv[i]));

    EXPECT_THAT(fuse_argv, Contains("-obig_writes"));
    EXPECT_THAT(fuse_argv, Contains("-f"));
    EXPECT_THAT(fuse_argv, Contains("-oopt"));
    EXPECT_THAT(fuse_argv, Contains("mountpoint"));
}

TEST_F(OptionsTest, describeCommandlineOptionsShouldReturnNotEmptyDescription)
{
    auto description = options.describeCommandlineOptions();
    EXPECT_FALSE(description.empty());
}
