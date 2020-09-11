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
    EXPECT_EQ(false, options.isInsecure());
    EXPECT_EQ(false, options.isIOTraceLoggerEnabled());
    EXPECT_EQ(false, options.isProxyIOForced());
    EXPECT_EQ(false, options.isDirectIOForced());
    EXPECT_EQ(false, options.isMonitoringEnabled());
    EXPECT_EQ(false, options.isMonitoringLevelFull());
    EXPECT_EQ(false, options.areFileReadEventsDisabled());
    EXPECT_EQ(true, options.isFullblockReadEnabled());
    EXPECT_EQ(true, options.isMonitoringLevelBasic());
    EXPECT_EQ(false, options.isClusterPrefetchThresholdRandom());
    EXPECT_EQ(0, options.getVerboseLogLevel());
    EXPECT_EQ(options::DEFAULT_PROVIDER_PORT, options.getProviderPort());
    EXPECT_EQ(options::DEFAULT_BUFFER_SCHEDULER_THREAD_COUNT,
        options.getBufferSchedulerThreadCount());
    EXPECT_EQ(options::DEFAULT_COMMUNICATOR_POOL_SIZE,
        options.getCommunicatorConnectionPoolSize());
    EXPECT_EQ(options::DEFAULT_COMMUNICATOR_THREAD_COUNT,
        options.getCommunicatorThreadCount());
    EXPECT_EQ(options::DEFAULT_SCHEDULER_THREAD_COUNT,
        options.getSchedulerThreadCount());
    EXPECT_EQ(options::DEFAULT_STORAGE_HELPER_THREAD_COUNT,
        options.getStorageHelperThreadCount());
    EXPECT_EQ(true, options.isIOBuffered());
    EXPECT_EQ(options::DEFAULT_PROVIDER_TIMEOUT,
        options.getProviderTimeout().count());
    EXPECT_EQ(
        options::DEFAULT_READ_BUFFER_MIN_SIZE, options.getReadBufferMinSize());
    EXPECT_EQ(
        options::DEFAULT_READ_BUFFER_MAX_SIZE, options.getReadBufferMaxSize());
    EXPECT_EQ(options::DEFAULT_READ_BUFFERS_TOTAL_SIZE,
        options.getReadBuffersTotalSize());
    EXPECT_EQ(options::DEFAULT_WRITE_BUFFERS_TOTAL_SIZE,
        options.getWriteBuffersTotalSize());
    EXPECT_EQ(options::DEFAULT_READ_BUFFER_PREFETCH_DURATION,
        options.getReadBufferPrefetchDuration().count());
    EXPECT_EQ(options::DEFAULT_WRITE_BUFFER_MIN_SIZE,
        options.getWriteBufferMinSize());
    EXPECT_EQ(options::DEFAULT_WRITE_BUFFER_MAX_SIZE,
        options.getWriteBufferMaxSize());
    EXPECT_EQ(options::DEFAULT_WRITE_BUFFER_FLUSH_DELAY,
        options.getWriteBufferFlushDelay().count());
    EXPECT_EQ(options::DEFAULT_MIN_BLOCK_PREFETCH_SIZE,
        options.getMinimumBlockPrefetchSize());
    EXPECT_EQ(
        options::DEFAULT_METADATA_CACHE_SIZE, options.getMetadataCacheSize());
    EXPECT_EQ(options::DEFAULT_READDIR_PREFETCH_SIZE,
        options.getReaddirPrefetchSize());
    EXPECT_EQ(options::DEFAULT_DIR_CACHE_DROP_AFTER,
        options.getDirectoryCacheDropAfter().count());
    EXPECT_EQ(1.0, options.getLinearReadPrefetchThreshold());
    EXPECT_EQ(1.0, options.getRandomReadPrefetchThreshold());
    EXPECT_EQ(options::DEFAULT_PREFETCH_CLUSTER_WINDOW_SIZE,
        options.getRandomReadPrefetchClusterWindow());
    EXPECT_EQ(options::DEFAULT_PREFETCH_MODE, options.getPrefetchMode());
    EXPECT_EQ(options::DEFAULT_PREFETCH_EVALUATE_FREQUENCY,
        options.getRandomReadPrefetchEvaluationFrequency());
    EXPECT_EQ(options::DEFAULT_PREFETCH_CLUSTER_BLOCK_THRESHOLD,
        options.getRandomReadPrefetchClusterBlockThreshold());
    EXPECT_EQ(0.0, options.getRandomReadPrefetchClusterWindowGrowFactor());
    EXPECT_EQ(0, options.getEmulateAvailableSpace());
    EXPECT_FALSE(options.getProviderHost());
    EXPECT_FALSE(options.getAccessToken());
    EXPECT_FALSE(options.isReadWritePerfEnabled());
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

TEST_F(OptionsTest, parseCommandLineShouldSetProviderHost)
{
    cmdArgs.insert(cmdArgs.end(), {"--host", "someHost", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("someHost", options.getProviderHost().get());
}

TEST_F(OptionsTest, parseCommandLineShouldSetSpaceNames)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--space", "Space1", "--space", "Space 2", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    std::vector<std::string> opts{"Space1", "Space 2"};
    EXPECT_EQ(opts, options.getSpaceNames());
}

TEST_F(OptionsTest, parseCommandLineShouldSetSpaceIds)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--space-id", "12345", "--space-id", "ABCDE", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    std::vector<std::string> opts{"12345", "ABCDE"};
    EXPECT_EQ(opts, options.getSpaceIds());
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
    EXPECT_EQ(true, options.isInsecure());
}

TEST_F(OptionsTest, parseCommandLineShouldSetInsecureDeprecated)
{
    cmdArgs.insert(cmdArgs.end(), {"--no_check_certificate", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.isInsecure());
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

TEST_F(OptionsTest, parseCommandLineShouldEnableIOTraceLog)
{
    cmdArgs.insert(cmdArgs.end(), {"--io-trace-log", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.isIOTraceLoggerEnabled());
}

TEST_F(OptionsTest, parseCommandLineShouldGetCustomLogLevels)
{
    cmdArgs.insert(cmdArgs.end(), {"--log-read-write-perf", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(options.isReadWritePerfEnabled(), true);
}

TEST_F(OptionsTest, parseCommandLineShouldSetForceProxyIO)
{
    cmdArgs.insert(cmdArgs.end(), {"--force-proxy-io", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.isProxyIOForced());
}

TEST_F(OptionsTest, parseCommandLineShouldSetForceDirectIO)
{
    cmdArgs.insert(cmdArgs.end(), {"--force-direct-io", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.isDirectIOForced());
}

TEST_F(OptionsTest, parseCommandLineShouldSetBufferSchedulerThreadCount)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--buffer-scheduler-thread-count", "8", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(8, options.getBufferSchedulerThreadCount());
}

TEST_F(OptionsTest, parseCommandLineShouldSetCommunicatorPoolSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--communicator-pool-size", "24", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(24, options.getCommunicatorConnectionPoolSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetCommunicatorThreadCount)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--communicator-thread-count", "8", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(8, options.getCommunicatorThreadCount());
}

TEST_F(OptionsTest, parseCommandLineShouldSetSchedulerThreadCount)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--scheduler-thread-count", "8", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(8, options.getSchedulerThreadCount());
}

TEST_F(OptionsTest, parseCommandLineShouldSetStorageHelperThreadCount)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--storage-helper-thread-count", "8", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(8, options.getStorageHelperThreadCount());
}

TEST_F(OptionsTest, parseCommandLineShouldSetNoBuffer)
{
    cmdArgs.insert(cmdArgs.end(), {"--no-buffer", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(false, options.isIOBuffered());
}

TEST_F(OptionsTest, parseCommandLineShouldSetDisableFileReadEvents)
{
    cmdArgs.insert(cmdArgs.end(), {"--disable-read-events", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.areFileReadEventsDisabled());
}

TEST_F(OptionsTest, parseCommandLineShouldEnableFullblockRead)
{
    cmdArgs.insert(cmdArgs.end(), {"--no-fullblock-read", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(false, options.isFullblockReadEnabled());
}

TEST_F(OptionsTest, parseCommandLineShouldSetProviderTimeout)
{
    cmdArgs.insert(cmdArgs.end(), {"--provider-timeout", "300", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(300, options.getProviderTimeout().count());
}

TEST_F(OptionsTest, parseCommandLineShouldSetReadBufferMinSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--read-buffer-min-size", "1024", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1024, options.getReadBufferMinSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetReadBufferMaxSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--read-buffer-max-size", "1024", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1024, options.getReadBufferMaxSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetReadBufferPrefetchDuration)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--read-buffer-prefetch-duration", "10", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(10, options.getReadBufferPrefetchDuration().count());
}

TEST_F(OptionsTest, parseCommandLineShouldSetWriteBufferMinSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--write-buffer-min-size", "1024", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1024, options.getWriteBufferMinSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetWriteBufferMaxSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--write-buffer-max-size", "1024", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1024, options.getWriteBufferMaxSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetReadBuffersTotalSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--read-buffers-total-size", "1024", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1024, options.getReadBuffersTotalSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetWriteBuffersTotalSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--write-buffers-total-size", "1024", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1024, options.getWriteBuffersTotalSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetWriteBufferFlushDelay)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--write-buffer-flush-delay", "10", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(10, options.getWriteBufferFlushDelay().count());
}

TEST_F(OptionsTest, parseCommandLineShouldSetMinPrefetchSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--min-block-prefetch-size", "1024", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1024, options.getMinimumBlockPrefetchSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetMetadataCacheSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--metadata-cache-size", "1024", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1024, options.getMetadataCacheSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetReaddirPrefetchSize)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--readdir-prefetch-size", "10000", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(10000, options.getReaddirPrefetchSize());
}

TEST_F(OptionsTest, parseCommandLineShouldSetDirCacheDropAfter)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--dir-cache-drop-after", "60", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(60, options.getDirectoryCacheDropAfter().count());
}

TEST_F(OptionsTest, parseCommandLineShouldSetTagOnCreate)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--tag-on-create", "KEY1:VALUE1", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    auto value = std::make_pair<std::string, std::string>("KEY1", "VALUE1");
    EXPECT_EQ(value, options.getOnCreateTag().get());
}

TEST_F(OptionsTest, parseCommandLineShouldSetTagOnModify)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--tag-on-modify", "KEY1:VALUE1", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    auto value = std::make_pair<std::string, std::string>("KEY1", "VALUE1");
    EXPECT_EQ(value, options.getOnModifyTag().get());
}

TEST_F(OptionsTest, parseCommandLineShouldOverrideHelperParam)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--override", "STORAGE1:PARAM1:VALUE1:2:3", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());

    EXPECT_EQ(
        "VALUE1:2:3", options.getHelperOverrideParams()["STORAGE1"]["PARAM1"]);
}

TEST_F(OptionsTest, parseCommandLineShouldOverrideMultipleHelperParams)
{
    cmdArgs.insert(cmdArgs.end(),
        {"-r", "STORAGE1:PARAM1:VALUE1", "-r", "STORAGE2:PARAM2:VALUE2", "-r",
            "STORAGE3:PARAM3:VALUE3", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());

    auto params = options.getHelperOverrideParams();

    EXPECT_EQ(
        "VALUE1", options.getHelperOverrideParams()["STORAGE1"]["PARAM1"]);
    EXPECT_EQ(
        "VALUE2", options.getHelperOverrideParams()["STORAGE2"]["PARAM2"]);
    EXPECT_EQ(
        "VALUE3", options.getHelperOverrideParams()["STORAGE3"]["PARAM3"]);
}

TEST_F(OptionsTest, parseCommandLineShouldSetLinearReadPrefetchTriggerThreshold)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--seqrd-prefetch-threshold", "0.3", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(0.3, options.getLinearReadPrefetchThreshold());
}

TEST_F(OptionsTest, parseCommandLineShouldSetRandomReadPrefetchTriggerThreshold)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--rndrd-prefetch-threshold", "0.3", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(0.3, options.getRandomReadPrefetchThreshold());
}

TEST_F(OptionsTest, parseCommandLineShouldSetRandomReadClusterWindow)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--rndrd-prefetch-cluster-window", "1024", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1024, options.getRandomReadPrefetchClusterWindow());
}

TEST_F(OptionsTest, parseCommandLineShouldSetPrefetchEvaluationFrequency)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--rndrd-prefetch-eval-frequency", "250", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(250, options.getRandomReadPrefetchEvaluationFrequency());
}

TEST_F(OptionsTest, parseCommandLineShouldSetFullFileRandomReadClusterWindow)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--rndrd-prefetch-cluster-window", "-1", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(-1, options.getRandomReadPrefetchClusterWindow());
}

TEST_F(OptionsTest, parseCommandLineShouldSetRandomReadClusterBlockThreshold)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--rndrd-prefetch-cluster-block-threshold", "10", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(10, options.getRandomReadPrefetchClusterBlockThreshold());
}

TEST_F(OptionsTest, parseCommandLineShouldSetRandomReadClusterWindowGrowFactor)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--rndrd-prefetch-cluster-window-grow-factor", "1.2", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1.2, options.getRandomReadPrefetchClusterWindowGrowFactor());
}

TEST_F(OptionsTest, parseCommandLineShouldSetEmulateAvailableSpace)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--emulate-available-space", "1125899906842624", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(1125899906842624ULL, options.getEmulateAvailableSpace());
}

TEST_F(OptionsTest, parseCommandLineShouldSetPrefetchMode)
{
    cmdArgs.insert(cmdArgs.end(), {"--prefetch-mode=sync", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ("sync", options.getPrefetchMode());
}

TEST_F(OptionsTest, parseCommandLineShouldSetClusterPrefetchThresholdRandom)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--cluster-prefetch-threshold-random", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.isClusterPrefetchThresholdRandom());
}

TEST_F(OptionsTest, parseCommandLineShouldSetRandomReadBlockThreshold)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--rndrd-prefetch-block-threshold", "150", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(150, options.getRandomReadPrefetchBlockThreshold());
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

TEST_F(OptionsTest, parseCommandLineShouldSetVerboseLogLevel)
{
    cmdArgs.insert(cmdArgs.end(), {"--verbose-log-level", "3", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(3, options.getVerboseLogLevel());
}

TEST_F(OptionsTest, parseCommandLineShouldSetSingleThread)
{
    cmdArgs.insert(cmdArgs.end(), {"--single-thread", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.getSingleThread());
}

TEST_F(OptionsTest, parseCommandLineShouldSetFuseOpts)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--opt", "someOpt0", "--opt", "someOpt1", "--opt", "someOpt2",
            "mountpoint"});
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
    EXPECT_EQ(shortOpts.isInsecure(), longOpts.isInsecure());
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

TEST_F(OptionsTest, parseEnvironmentShouldSetProviderHostDeprecated)
{
    setenv("PROVIDER_HOSTNAME", "someHost", true);
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
    EXPECT_EQ(true, options.isInsecure());
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

TEST_F(OptionsTest, parseConfigFileShouldSetProviderHostDeprecated)
{
    setInConfigFile("provider_hostname", "someHost");
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
    EXPECT_EQ(true, options.isInsecure());
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

TEST_F(OptionsTest, parseConfigFileShouldSetForceProxyIO)
{
    setInConfigFile("force_proxy_io", "1");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(true, options.isProxyIOForced());
}

TEST_F(OptionsTest, parseConfigFileShouldSetForceDirectIO)
{
    setInConfigFile("force_direct_io", "1");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(true, options.isDirectIOForced());
}

TEST_F(OptionsTest, parseConfigFileShouldSetBufferSchedulerThreadCount)
{
    setInConfigFile("buffer_scheduler_thread_count", "8");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(8, options.getBufferSchedulerThreadCount());
}

TEST_F(OptionsTest, parseConfigFileShouldSetCommunicatorPoolSize)
{
    setInConfigFile("communicator_pool_size", "24");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(24, options.getCommunicatorConnectionPoolSize());
}

TEST_F(OptionsTest, parseConfigFileShouldSetCommunicatorThreadCount)
{
    setInConfigFile("communicator_thread_count", "8");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(8, options.getCommunicatorThreadCount());
}

TEST_F(OptionsTest, parseConfigFileShouldSetSchedulerThreadCount)
{
    setInConfigFile("scheduler_thread_count", "8");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(8, options.getSchedulerThreadCount());
}

TEST_F(OptionsTest, parseConfigFileShouldSetStorageHelperThreadCount)
{
    setInConfigFile("storage_helper_thread_count", "8");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(8, options.getStorageHelperThreadCount());
}

TEST_F(OptionsTest, parseConfigFileShouldSetNoBuffer)
{
    setInConfigFile("no_buffer", "1");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(false, options.isIOBuffered());
}

TEST_F(OptionsTest, parseConfigFileShouldSetProviderTimeout)
{
    setInConfigFile("provider_timeout", "300");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(300, options.getProviderTimeout().count());
}

TEST_F(OptionsTest, parseConfigFileShouldSetReadBufferMinSize)
{
    setInConfigFile("read_buffer_min_size", "1024");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(1024, options.getReadBufferMinSize());
}

TEST_F(OptionsTest, parseConfigFileShouldSetReadBufferMaxSize)
{
    setInConfigFile("read_buffer_max_size", "1024");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(1024, options.getReadBufferMaxSize());
}

TEST_F(OptionsTest, parseConfigFileShouldSetReadBufferPrefetchDuration)
{
    setInConfigFile("read_buffer_prefetch_duration", "10");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(10, options.getReadBufferPrefetchDuration().count());
}

TEST_F(OptionsTest, parseConfigFileShouldSetWriteBufferMinSize)
{
    setInConfigFile("write_buffer_min_size", "1024");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(1024, options.getWriteBufferMinSize());
}

TEST_F(OptionsTest, parseConfigFileShouldSetWriteBufferMaxSize)
{
    setInConfigFile("write_buffer_max_size", "1024");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(1024, options.getWriteBufferMaxSize());
}

TEST_F(OptionsTest, parseConfigFileShouldSetWriteBufferFlushDelay)
{
    setInConfigFile("write_buffer_flush_delay", "10");
    options.parse(fileArgs.size(), fileArgs.data());
    EXPECT_EQ(10, options.getWriteBufferFlushDelay().count());
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

TEST_F(OptionsTest, parseCommandLineShouldEnableMonitoringWithType)
{
    cmdArgs.insert(
        cmdArgs.end(), {"--monitoring-type", "graphite", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.isMonitoringEnabled());
    EXPECT_TRUE(options.getMonitoringType().get() == "graphite");
}

TEST_F(OptionsTest, parseCommandLineShouldEnableMonitoringLevelFull)
{
    cmdArgs.insert(cmdArgs.end(), {"--monitoring-level-full", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_EQ(true, options.isMonitoringLevelFull());
}

TEST_F(OptionsTest, parseCommandLineShouldReturnGraphiteUrl)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--graphite-url", "tcp://graphite.example.com:2003", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_TRUE(options.getMonitoringGraphiteUrl().get() ==
        "tcp://graphite.example.com:2003");
}

TEST_F(OptionsTest, parseCommandLineShouldReturnGraphiteiNamespacePrefix)
{
    cmdArgs.insert(cmdArgs.end(),
        {"--graphite-namespace-prefix", "DataCenterA", "mountpoint"});
    options.parse(cmdArgs.size(), cmdArgs.data());
    EXPECT_TRUE(
        options.getMonitoringGraphiteNamespacePrefix().get() == "DataCenterA");
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
    cmdArgs.insert(cmdArgs.end(),
        {"--host", "someHost1", "--config", configFilePath.c_str(),
            "mountpoint"});
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
