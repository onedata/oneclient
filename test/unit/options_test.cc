/**
 * @file options_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "options/options.h"

#include <gtest/gtest.h>

#include <fstream>
#include <iostream>
#include <vector>

using namespace ::testing;

struct OptionsTest : public ::testing::Test {
    OptionsTest()
        : configFilePath{boost::filesystem::temp_directory_path() /
              boost::filesystem::unique_path()}
        , cmdArgs{"oneclient"}
        , envArgs{"oneclient", "mountpoint"}
        , fileArgs{"oneclient", "-c", configFilePath.c_str(), "mountpoint"}
    {
    }

    ~OptionsTest()
    {
        for (const std::string &env :
            {"CONFIG", "PROVIDER_HOST", "PROVIDER_PORT", "INSECURE",
                "ACCESS_TOKEN", "AUTHORIZATION_TOKEN", "LOG_DIR",
                "FUSE_FOREGROUND", "FUSE_DEBUG", "FUSE_SINGLE_THREAD",
                "FUSE_MOUNT_OPT", "FUSE_MOUNTPOINT"}) {
            unsetenv(env.c_str());
            unsetenv(("ONECLIENT_" + env).c_str());
        }

        boost::system::error_code ec;
        boost::filesystem::remove_all(configFilePath, ec);
    }

    void setInConfigFile(const std::string &key, const std::string &value)
    {
        std::ofstream configFile;
        configFile.open(configFilePath.c_str(), std::ios_base::app);
        configFile << key << " = " << value << std::endl;
        configFile.close();
    }

    boost::filesystem::path configFilePath;
    std::vector<const char *> cmdArgs;
    std::vector<const char *> envArgs;
    std::vector<const char *> fileArgs;
    one::client::options::Options options{};
};

TEST_F(OptionsTest, formatHelpShouldReturnNonemptyString)
{
    EXPECT_FALSE(options.formatHelp("oneclient").empty());
}

TEST_F(OptionsTest, formatDeprecatedShouldReturnEmptyString)
{
    EXPECT_TRUE(options.formatDeprecated().empty());
}

TEST_F(OptionsTest, parseCommandLineShouldFailWhenMountpointIsMissing)
{
    cmdArgs.push_back("--foreground");
    EXPECT_THROW(options.parse(cmdArgs.size(), cmdArgs.data()),
        boost::program_options::error);
    cmdArgs.push_back("mountpoint");
    options.parse(cmdArgs.size(), cmdArgs.data());
}

TEST_F(OptionsTest, getOptionShouldReturnDefaultValue)
{
    using namespace one::client;

    cmdArgs.push_back("mountpoint");
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(false, options.getHelp());
    EXPECT_EQ(false, options.getVersion());
    EXPECT_EQ(false, options.getUnmount());
    EXPECT_EQ(false, options.getForeground());
    EXPECT_EQ(false, options.getDebug());
    EXPECT_EQ(false, options.getSingleThread());
    EXPECT_EQ(false, options.getInsecure());
    EXPECT_EQ(options::DEFAULT_PROVIDER_PORT, options.getProviderPort());
    EXPECT_FALSE(options.getProviderHost());
    EXPECT_FALSE(options.getAccessToken());
}

TEST_F(OptionsTest, parseCommandLineShouldSetHelpWhenNoArguments)
{
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getHelp());
}

TEST_F(OptionsTest, parseCommandLineShouldSetMountpoint)
{
    cmdArgs.push_back("somePath");
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("somePath", options.getMountpoint());
}

TEST_F(OptionsTest, parseCommandLineShouldSetHelp)
{
    cmdArgs.push_back("--help");
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getHelp());
}

TEST_F(OptionsTest, parseCommandLineShouldSetVersion)
{
    cmdArgs.push_back("--version");
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getVersion());
}

TEST_F(OptionsTest, parseCommandLineShouldSetUnmount)
{
    cmdArgs.insert(cmdArgs.end(), {"--unmount", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getUnmount());
}

TEST_F(OptionsTest, parseCommandLineShouldSetForeground)
{
    cmdArgs.insert(cmdArgs.end(), {"--foreground", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getForeground());
}

TEST_F(OptionsTest, parseCommandLineShouldSetDebug)
{
    cmdArgs.insert(cmdArgs.end(), {"--debug", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getDebug());
}

TEST_F(OptionsTest, parseCommandLineShouldSetSingleThread)
{
    cmdArgs.insert(cmdArgs.end(), {"--single-thread", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getSingleThread());
}

TEST_F(OptionsTest, parseCommandLineShouldSetProviderHost)
{
    cmdArgs.insert(cmdArgs.end(), {"--host", "someHost", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("someHost", options.getProviderHost().get());
}

TEST_F(OptionsTest, parseCommandLineShouldSetProviderPort)
{
    cmdArgs.insert(cmdArgs.end(), {"--port", "1234", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1234, options.getProviderPort());
}

TEST_F(OptionsTest, parseCommandLineShouldSetAccessToken)
{
    cmdArgs.insert(cmdArgs.end(), {"--token", "someToken", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("someToken", options.getAccessToken().get());
}

TEST_F(OptionsTest, parseCommandLineShouldSetInsecure)
{
    cmdArgs.insert(cmdArgs.end(), {"--insecure", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getInsecure());
}

TEST_F(OptionsTest, parseCommandLineShouldSetInsecureDeprecated)
{
    cmdArgs.insert(cmdArgs.end(), {"--no_check_certificate", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getInsecure());
}

TEST_F(OptionsTest, parseCommandLineShouldSetConfigFilePath)
{
    cmdArgs.insert(cmdArgs.end(), {"--config", "somePath", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("somePath", options.getConfigFilePath());
}

TEST_F(OptionsTest, parseCommandLineShouldSetLogDirPath)
{
    cmdArgs.insert(cmdArgs.end(), {"--log-dir", "somePath", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("somePath", options.getLogDirPath());
}

TEST_F(OptionsTest, parseCommandLineShouldSetFuseOpts)
{
    cmdArgs.insert(cmdArgs.end(), {"--opt", "someOpt0", "--opt", "someOpt1",
                                      "--opt", "someOpt2", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    auto opts = options.getFuseOpts();
    std::sort(opts.begin(), opts.end());
    EXPECT_EQ(3, opts.size());
    EXPECT_EQ("someOpt0", opts[0]);
    EXPECT_EQ("someOpt1", opts[1]);
    EXPECT_EQ("someOpt2", opts[2]);
}

TEST_F(OptionsTest, parseCommandLineShouldSetFuseArgs)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--foreground", "--debug", "--single-thread", "--opt", "someOpt0",
            "--opt", "someOpt1", "--opt", "someOpt2", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(9, options.getFuseArgs("oneclient").argc);
}

TEST_F(OptionsTest, parseCommandLineShouldWarnOnDeprecatedOptions)
{
    cmdArgs.insert(cmdArgs.end(), {"--no_check_certificate", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_TRUE(options.hasDeprecated());
    EXPECT_FALSE(options.formatDeprecated().empty());
}

TEST_F(OptionsTest, shortCommandLineOptionsShouldBeInterchangeableWithLong)
{
    std::vector<const char *> shortArgs{"oneclient", "-u", "-f", "-d", "-s",
        "-H", "someHost", "-P", "1234", "-t", "someToken", "-i", "-c",
        "someFileConfigPath", "-l", "someLogDirPath", "--opt", "someOpt",
        "mountpoint"};
    one::client::options::Options shortOpts{};
    shortOpts.parse(shortArgs.size(), shortArgs.data());

    std::vector<const char *> longArgs{"oneclient", "--unmount", "--foreground",
        "--debug", "--single-thread", "--host", "someHost", "--port", "1234",
        "--token", "someToken", "--insecure", "--config", "someFileConfigPath",
        "--log-dir", "someLogDirPath", "-o", "someOpt", "mountpoint"};
    one::client::options::Options longOpts{};
    longOpts.parse(longArgs.size(), longArgs.data());

    EXPECT_EQ(shortOpts.getUnmount(), longOpts.getUnmount());
    EXPECT_EQ(shortOpts.getForeground(), longOpts.getForeground());
    EXPECT_EQ(shortOpts.getDebug(), longOpts.getDebug());
    EXPECT_EQ(shortOpts.getSingleThread(), longOpts.getSingleThread());
    EXPECT_EQ(shortOpts.getProviderHost(), longOpts.getProviderHost());
    EXPECT_EQ(shortOpts.getProviderPort(), longOpts.getProviderPort());
    EXPECT_EQ(shortOpts.getAccessToken(), longOpts.getAccessToken());
    EXPECT_EQ(shortOpts.getInsecure(), longOpts.getInsecure());
    EXPECT_EQ(shortOpts.getConfigFilePath(), longOpts.getConfigFilePath());
    EXPECT_EQ(shortOpts.getLogDirPath(), longOpts.getLogDirPath());
    EXPECT_EQ(shortOpts.getMountpoint(), longOpts.getMountpoint());
    EXPECT_EQ(shortOpts.getFuseOpts(), longOpts.getFuseOpts());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetConfigFilePath)
{
    setenv("ONECLIENT_CONFIG", "somePath", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ("somePath", options.getConfigFilePath());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetProviderHost)
{
    setenv("ONECLIENT_PROVIDER_HOST", "someHost", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ("someHost", options.getProviderHost().get());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetProviderPort)
{
    setenv("ONECLIENT_PROVIDER_PORT", "1234", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ(1234, options.getProviderPort());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetInsecure)
{
    setenv("ONECLIENT_INSECURE", "1", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ(true, options.getInsecure());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetToken)
{
    setenv("ONECLIENT_ACCESS_TOKEN", "someToken", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ("someToken", options.getAccessToken().get());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetTokenDeprecated)
{
    setenv("AUTHORIZATION_TOKEN", "someToken", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ("someToken", options.getAccessToken().get());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetLogDirPath)
{
    setenv("ONECLIENT_LOG_DIR", "somePath", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ("somePath", options.getLogDirPath());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetForeground)
{
    setenv("ONECLIENT_FUSE_FOREGROUND", "1", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ(true, options.getForeground());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetDebug)
{
    setenv("ONECLIENT_FUSE_DEBUG", "1", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ(true, options.getDebug());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetSingleThread)
{
    setenv("ONECLIENT_FUSE_SINGLE_THREAD", "1", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ(true, options.getSingleThread());
}

TEST_F(OptionsTest, parseEnvironmentShouldSetMountpoint)
{
    cmdArgs.push_back("--insecure");
    setenv("ONECLIENT_MOUNTPOINT", "somePath", true);
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("somePath", options.getMountpoint());
}

TEST_F(OptionsTest, parseEnvironmentShouldAcceptEnvsWithoutPrefix)
{
    setenv("CONFIG", "somePath", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_EQ("somePath", options.getConfigFilePath());
}

TEST_F(OptionsTest, parseEnvironmentShouldWarnOnEnvsWithoutPrefix)
{
    setenv("CONFIG", "somePath", true);
    options.parse(envArgs.size(), envArgs.data());
    EXPECT_TRUE(options.hasDeprecated());
    EXPECT_FALSE(options.formatDeprecated().empty());
}

TEST_F(OptionsTest, parseConfigFileShouldSetProviderHost)
{
    setInConfigFile("provider_host", "someHost");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ("someHost", options.getProviderHost().get());
}

TEST_F(OptionsTest, parseConfigFileShouldSetProviderPort)
{
    setInConfigFile("provider_port", "1234");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(1234, options.getProviderPort());
}

TEST_F(OptionsTest, parseConfigFileShouldSetInsecure)
{
    setInConfigFile("insecure", "1");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(true, options.getInsecure());
}

TEST_F(OptionsTest, parseConfigFileShouldSetAccessToken)
{
    setInConfigFile("access_token", "someToken");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ("someToken", options.getAccessToken().get());
}

TEST_F(OptionsTest, parseConfigFileShouldSetLogDir)
{
    setInConfigFile("log_dir", "somePath");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ("somePath", options.getLogDirPath());
}

TEST_F(OptionsTest, parseConfigFileShouldSetForeground)
{
    setInConfigFile("fuse_foreground", "1");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(true, options.getForeground());
}

TEST_F(OptionsTest, parseConfigFileShouldSetDebug)
{
    setInConfigFile("fuse_debug", "1");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(true, options.getDebug());
}

TEST_F(OptionsTest, parseConfigFileShouldSetSingleThread)
{
    setInConfigFile("fuse_single_thread", "1");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(true, options.getSingleThread());
}

TEST_F(OptionsTest, parseConfigFileShouldSetFuseOpts)
{
    setInConfigFile("fuse_mount_opt", "someOpt0");
    setInConfigFile("fuse_mount_opt", "someOpt1");
    setInConfigFile("fuse_mount_opt", "someOpt2");
    options.parse(fileArgs.size(), fileArgs.data());
    auto opts = options.getFuseOpts();
    std::sort(opts.begin(), opts.end());
    EXPECT_EQ(3, opts.size());
    EXPECT_EQ("someOpt0", opts[0]);
    EXPECT_EQ("someOpt1", opts[1]);
    EXPECT_EQ("someOpt2", opts[2]);
}

TEST_F(OptionsTest, parseConfigFileShouldSetMountpoint)
{
    cmdArgs.insert(cmdArgs.end(), {"-c", configFilePath.c_str()});
    setInConfigFile("mountpoint", "somePath");
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("somePath", options.getMountpoint());
}

TEST_F(OptionsTest, parseShouldSetOptionsInOrder)
{
    cmdArgs.insert(cmdArgs.end(), {"--host", "someHost1", "--config",
                                      configFilePath.c_str(), "mountpoint"});
    setenv("ONECLIENT_PROVIDER_HOST", "someHost2", true);
    setInConfigFile("provider_host", "someHost3");

    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("someHost1", options.getProviderHost().get());

    options = one::client::options::Options{};
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ("someHost2", options.getProviderHost().get());

    unsetenv("ONECLIENT_PROVIDER_HOST");
    options = one::client::options::Options{};
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ("someHost3", options.getProviderHost().get());
}
