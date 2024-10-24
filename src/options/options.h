/**
 * @file options.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_OPTIONS_H
#define ONECLIENT_OPTIONS_H

#include "helpers/logging.h"
#include "messages/common.h"

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/filesystem.hpp>
#include <boost/optional.hpp>
#include <boost/program_options.hpp>
#include <folly/Optional.h>

#if FUSE_USE_VERSION > 30
#include <fuse3/fuse_opt.h>
#else
#include <fuse/fuse_opt.h>
#endif

#include <chrono>
#include <vector>

namespace one {
namespace client {
namespace options {

namespace {
static constexpr auto CONFIG_FILE_NAME = "oneclient.conf";
static constexpr auto ONES3_CONFIG_FILE_NAME = "ones3.conf";
static constexpr auto ENVIRONMENT_PREFIX = "ONECLIENT_";
static constexpr auto DEFAULT_PROVIDER_PORT = 443;
static constexpr auto DEFAULT_BUFFER_SCHEDULER_THREAD_COUNT = 1;
static constexpr auto DEFAULT_COMMUNICATOR_POOL_SIZE = 25;
static constexpr auto DEFAULT_COMMUNICATOR_THREAD_COUNT = 1;
static constexpr auto DEFAULT_SCHEDULER_THREAD_COUNT = 1;
static constexpr auto DEFAULT_STORAGE_HELPER_THREAD_COUNT = 10;
static constexpr auto DEFAULT_READ_BUFFER_MIN_SIZE = 4 * 1024;
static constexpr auto DEFAULT_READ_BUFFER_MAX_SIZE = 100 * 1024 * 1024;
static constexpr auto DEFAULT_READ_BUFFERS_TOTAL_SIZE =
    20 * DEFAULT_READ_BUFFER_MAX_SIZE;
static constexpr auto DEFAULT_READ_BUFFER_PREFETCH_DURATION = 1;
static constexpr auto DEFAULT_WRITE_BUFFER_MIN_SIZE = 20 * 1024 * 1024;
static constexpr auto DEFAULT_WRITE_BUFFER_MAX_SIZE = 50 * 1024 * 1024;
static constexpr auto DEFAULT_WRITE_BUFFERS_TOTAL_SIZE =
    20 * DEFAULT_WRITE_BUFFER_MAX_SIZE;
static constexpr auto DEFAULT_WRITE_BUFFER_FLUSH_DELAY = 5;
static constexpr auto DEFAULT_PREFETCH_MODE = "async";
static constexpr auto DEFAULT_PREFETCH_EVALUATE_FREQUENCY = 50;
static constexpr double DEFAULT_PREFETCH_POWER_BASE = 1.3;
static constexpr auto DEFAULT_PREFETCH_TARGET_LATENCY =
    std::chrono::nanoseconds{1000}; // NOLINT
static constexpr auto DEFAULT_MIN_BLOCK_PREFETCH_SIZE = 1024 * 1024u;
static constexpr auto DEFAULT_PREFETCH_CLUSTER_WINDOW_SIZE = 20971520;
static constexpr auto DEFAULT_PREFETCH_CLUSTER_BLOCK_THRESHOLD = 5;
static constexpr auto DEFAULT_METADATA_CACHE_SIZE = 5'000'000;
static constexpr auto DEFAULT_READDIR_PREFETCH_SIZE = 2500;
static constexpr auto DEFAULT_DIR_CACHE_DROP_AFTER = 5 * 60;
static constexpr auto DEFAULT_DIR_CACHE_DROP_AFTER_IN_OPEN_SHARE_MODE = 10;
static constexpr auto DEFAULT_PROVIDER_TIMEOUT = 2 * 60;
static constexpr auto DEFAULT_STORAGE_TIMEOUT = 2 * 60;
static constexpr auto DEFAULT_MONITORING_PERIOD_SECONDS = 30;
static constexpr auto DEFAULT_ONES3_STORAGE_SUPPORT_SIZE =
    10 * 1024 * 1024 * 1024ULL;
static constexpr auto DEFAULT_ONES3_MAX_BODY_SIZE = 128 * 1024 * 1024ULL;
static constexpr auto DEFAULT_ONES3_MAX_BODY_MEMORY_SIZE = 16 * 1024 * 1024ULL;
static constexpr auto DEFAULT_ONES3_IDLE_CONNECTION_TIMEOUT = 180;
static constexpr auto DEFAULT_ONES3_KEEPALIVE_REQUESTS_MAX = 1024;
static constexpr auto DEFAULT_ONES3_GET_STREAM_THRESHOLD = 2 * 1024 * 1024ULL;
static constexpr auto DEFAULT_ONES3_FILE_MODE = "0664";
static constexpr auto DEFAULT_ONES3_BUCKET_SPACEID_CACHE_EXPIRATION_SECONDS =
    std::chrono::seconds{30};

#if defined(__APPLE__)
static constexpr auto DEFAULT_EMULATE_AVAILABLE_SPACE =
    1024 * 1024 * 1024 * 1024ULL;
#else
static constexpr auto DEFAULT_EMULATE_AVAILABLE_SPACE = 0ULL;
#endif
}

class Option;
template <typename T> class TypedOption;
enum class OptionGroup;

/**
 * @c Options is responsible for storing and providing access to the client
 * options parsed from a command line, an environment and a configuration file.
 */
class Options {
public:
    /*
     * Constructor.
     * Builds list of supported client options.
     */
    Options(messages::handshake::ClientType clientType =
                messages::handshake::ClientType::oneclient);

    virtual ~Options() = default;

    /*
     * Parses options from command line, environment and configuration file.
     * If 'help' or 'version' option is present in command line options parsing
     * is interupted.
     * @param argc Number of command line arguments.
     * @param argv An array of command line arguments.
     */
    void parse(const int argc, const char *const argv[]);

    /*
     * @return true if in parsed options there are deprecated ones, otherwise
     * false.
     */
    bool hasDeprecated();

    /*
     * @return Formatted warning message about deprecated options.
     */
    std::string formatDeprecated() const;

    /*
     * @param programName Name of the client program.
     * @return Formatted help message.
     */
    std::string formatHelp(const char *programName) const;
    std::string formatHelpOneS3(const char *programName) const;

    /*
     * @return true if 'help' option has been provided or if arguments list was
     * empty, otherwise false.
     */
    bool getHelp() const;

    /*
     * @return true if 'version' option has been provided, otherwise false.
     */
    bool getVersion() const;

    /*
     * @return true if 'unmount' option has been provided, otherwise false.
     */
    bool getUnmount() const;

    /*
     * @return true if 'ignore-env' option has been provided, otherwise false.
     */
    bool isIgnoreEnv() const;

    /*
     * @return true if 'foreground' option has been provided, otherwise false.
     */
    bool getForeground() const;

    /*
     * @return true if 'debug' option has been provided, otherwise false.
     */
    bool getDebug() const;

    /*
     * @return Get verbose log level.
     */
    unsigned int getVerboseLogLevel() const;

    /**
     * @return If true, logs will buffering will be disabled.
     */
    bool disableLogBuffering() const;

    /*
     * @return true if 'single-thread' option has been provided, otherwise
     * false.
     */
    bool getSingleThread() const;

    /*
     * @return Provider hostname if option has been provided.
     */
    boost::optional<std::string> getProviderHost() const;

    /*
     * @return Onezone hostname if option has been provided.
     */
    boost::optional<std::string> getOnezoneHost() const;

    boost::optional<boost::filesystem::path> getCustomCACertificateDir() const;

    /*
     * @return OneS3 readiness probe authentication if option has been provided.
     */
    boost::optional<std::string> getOneS3ReadinessProbeBasicAuth() const;

    std::string getOneS3AddressBind() const;

    /*
     * @return OneS3 HTTP port - if provided.
     */
    boost::optional<unsigned int> getOneS3HTTPPort() const;

    /*
     * @return OneS3 HTTPS port - if provided.
     */
    boost::optional<unsigned int> getOneS3HTTPSPort() const;

    /*
     * @return Path to OneS3 SSL certificate n PEM format.
     */
    boost::optional<boost::filesystem::path> getOneS3SSLCertificatePath() const;

    /*
     * @return Path to OneS3 SSL key in PEM format.
     */
    boost::optional<boost::filesystem::path> getOneS3SSLKeyPath() const;

    /**
     * @return Number of OneS3 HTTP server threads.
     */
    unsigned int getOneS3ThreadNum() const;

    /**
     *
     * @return Number of maximum simultaneous keepalive requests.
     */
    unsigned int getOneS3KeepaliveRequests() const;

    /**
     *
     * @return Maximum OneS3 put request body size in bytes.
     */
    size_t getOneS3MaxBodySize() const;

    /**
     *
     * @return Maximum OneS3 put request body size that will be handled in
     * memory.
     */
    size_t getOneS3MaxBodyMemorySize() const;

    /**
     *
     * @return Maximum GET request content-length size that will be returned
     * in a single read. Larger requests will be streamed.
     */
    size_t getOneS3StreamGetThreshold() const;

    /**
     *
     * @return OneS3 idle connection timeout in seconds.
     */
    unsigned int getOneS3IdleConnectionTimeout() const;

    /**
     * @return POSIX file mode for new files.
     */
    int getOneS3FileMode() const;

    /*
     * @return Provider port.
     */
    unsigned int getProviderPort() const;

    /*
     * @return true if 'insecure' option has been provided, otherwise false.
     */
    bool isInsecure() const;

    /*
     * @return Access token if option has been provided.
     */
    virtual boost::optional<std::string> getAccessToken() const;

    /*
     * @return Configuration file path.
     */
    boost::filesystem::path getConfigFilePath() const;

    /*
     * @return Log directory path.
     */
    boost::filesystem::path getLogDirPath() const;

    /*
     * @return True if IO trace log is enabled.
     */
    bool isIOTraceLoggerEnabled() const;

    /*
     * @return True if message trace log is enabled.
     */
    bool isMessageTraceLoggerEnabled() const;

    /*
     * @return True if read write perf is enabled.
     */
    bool isReadWritePerfEnabled() const;

    /*
     * @return true if 'force-proxy-io' option has been provided, otherwise
     * false.
     */
    bool isProxyIOForced() const;

    /*
     * @return true if 'force-direct-io' option has been provided, otherwise
     * false.
     */
    bool isDirectIOForced() const;

    /*
     * @return Number of parallel buffer scheduler threads.
     */
    unsigned int getBufferSchedulerThreadCount() const;

    /*
     * @return Number of connections in communicator connection pool.
     */
    unsigned int getCommunicatorConnectionPoolSize() const;

    /*
     * @return Number of parallel communicator threads.
     */
    unsigned int getCommunicatorThreadCount() const;

    /*
     * @return Number of parallel scheduler threads.
     */
    unsigned int getSchedulerThreadCount() const;

    /*
     * @return Number of parallel storage helper threads.
     */
    unsigned int getStorageHelperThreadCount() const;

    /*
     * @return true if 'disable-read-events' is specified.
     */
    bool areFileReadEventsDisabled() const;

    /*
     * @return false if 'no-fullblock-read' is specified.
     */
    bool isFullblockReadEnabled() const;

    /*
     * @return false if 'no-buffer' option has been provided, otherwise true.
     */
    bool isIOBuffered() const;

    /*
     * @return false if 'no-xattr' option has been provided, otherwise true.
     */
    bool enableExtendedAttributes() const;

    /*
     * @return Return timeout for Oneprovider communication.
     */
    std::chrono::seconds getProviderTimeout() const;

    /*
     * @return Return timeout for storage operations.
     */
    std::chrono::seconds getStorageTimeout() const;

    /*
     * @return Minimum size in bytes of in-memory cache for input data blocks.
     */
    unsigned int getReadBufferMinSize() const;

    /*
     * @return Maximum size in bytes of in-memory cache for input data blocks.
     */
    unsigned int getReadBufferMaxSize() const;

    /*
     * @return Read ahead period in seconds of in-memory cache for input data
     * blocks.
     */
    std::chrono::seconds getReadBufferPrefetchDuration() const;

    /*
     * @return Minimum size in bytes of in-memory cache for output data blocks.
     */
    unsigned int getWriteBufferMinSize() const;

    /*
     * @return Maximum size in bytes of in-memory cache for output data blocks.
     */
    unsigned int getWriteBufferMaxSize() const;

    /*
     * @return Total possible memory size for read buffers (sum of all buffer
     * maximum sizes).
     */
    unsigned int getReadBuffersTotalSize() const;

    /*
     * @return Total possible memory size for write buffers (sum of all buffer
     * maximum sizes).
     */
    unsigned int getWriteBuffersTotalSize() const;

    /*
     * @return Idle period in seconds before flush of in-memory cache for
     * output data blocks.
     */
    std::chrono::seconds getWriteBufferFlushDelay() const;

    /*
     * @return The minimum prefetch block size. If a read for a block smaller
     * than this is requested, and the data is not available locally and needs
     * to be fetched from remote provider, the client will request to fetch this
     * amount anyway.
     */
    unsigned int getMinimumBlockPrefetchSize() const;

    /*
     * @return The linear read prefetch threshold trigger in (0.0-1.0]
     */
    double getLinearReadPrefetchThreshold() const;

    /*
     * @return The random read prefetch threshold trigger in (0.0-1.0]
     */
    double getRandomReadPrefetchThreshold() const;

    /*
     * @return The random read prefetch calculation will be performed on every N
     * reads.
     */
    unsigned int getRandomReadPrefetchEvaluationFrequency() const;

    /*
     * @return Get prefetch mode sync or async.
     */
    std::string getPrefetchMode() const;

    /*
     * @return Is cluster block prefetch threshold random.
     */
    bool isClusterPrefetchThresholdRandom() const;

    /*
     * @return Absolute number of blocks in file location before prefetch is
     * triggered, 0 disables the limit.
     */
    unsigned int getRandomReadPrefetchBlockThreshold() const;

    /*
     * @return Cluster window size for random read block prefetch.
     */
    int getRandomReadPrefetchClusterWindow() const;

    /*
     * @return The number of distinct blocks within cluster window
     * which will trigger prefetch of entire window.
     */
    unsigned int getRandomReadPrefetchClusterBlockThreshold() const;

    /*
     * @return Cluster window size grow factor for random read block prefetch.
     */
    double getRandomReadPrefetchClusterWindowGrowFactor() const;

    /*
     * @return Maximum number of entries in metadata cache.
     */
    unsigned int getMetadataCacheSize() const;

    /*
     * @return Set readdir cache prefetch size.
     */
    unsigned int getReaddirPrefetchSize() const;

    /*
     * @return Get size of emulated available space.
     */
    uint64_t getEmulateAvailableSpace() const;

    /*
     * @return Duration after which directory cache entries are dropped.
     */
    std::chrono::seconds getDirectoryCacheDropAfter() const;

    /*
     * @return Get xattr on-modify tag.
     */
    boost::optional<std::pair<std::string, std::string>> getOnModifyTag() const;

    /*
     * @return Get xattr on-create tag.
     */
    boost::optional<std::pair<std::string, std::string>> getOnCreateTag() const;

    /*
     * @return Get helper parameter override value
     */
    std::map<folly::fbstring,
        std::unordered_map<folly::fbstring, folly::fbstring>>
    getHelperOverrideParams() const;

    /*
     * @return Get helper parameter override values for specific storageId
     *         or empty map in case no overrides for this storage were
     *         provided
     */
    std::unordered_map<folly::fbstring, folly::fbstring>
    getHelperOverrideParams(const folly::fbstring &storageId) const;

    /**
     * @return Should client show only fully replicated files?
     */
    bool showOnlyFullReplicas() const;

    /**
     * @return Show number of hard links
     */
    bool showHardLinkCount() const;

    /**
     * @return Is Archivematica mode enabled.
     */
    bool isArchivematicaModeEnabled() const;

    /**
     * @return Is open share browsing mode enabled.
     */
    bool isOpenSharesModeEnabled() const;

    /**
     * @return Show space ids instead of names in filesystem tree.
     */
    bool showSpaceIds() const;

    /*
     * @return Is monitoring enabled.
     */
    bool isMonitoringEnabled() const;

    /*
     * @return Type of performance monitoring reporter.
     */
    boost::optional<std::string> getMonitoringType() const;

    /*
     * @return Monitoring reporting level basic flag.
     */
    bool isMonitoringLevelBasic() const;

    /*
     * @return Monitoring reporting level full flag.
     */
    bool isMonitoringLevelFull() const;

    /*
     * @return Performance monitoring Graphite URL.
     */
    boost::optional<std::string> getMonitoringGraphiteUrl() const;

    /*
     * @return Performance monitoring Graphite namespace prefix.
     */
    boost::optional<std::string> getMonitoringGraphiteNamespacePrefix() const;

    /*
     * @return Performance monitoring reporting period.
     */
    unsigned int getMonitoringReportingPeriod() const;

    boost::optional<std::string> getOneS3SupportAdminGroupId() const;

    size_t getOneS3SupportStorageSize() const;

    boost::optional<std::string> getOneS3SupportStorageCredentials() const;

    boost::optional<std::string> getOneS3SupportStorageId() const;

    std::chrono::seconds getOneS3BucketIdCacheExpirationTime() const;

    bool isOneS3BucketIdCacheExpirationAbsolute() const;

    /**
     * @brief Check, if bucket create and delete operations should be disabled.
     * @return True, if bucket create and delete operations should be disabled.
     */
    bool areOneS3BucketOperationsDisabled() const;

    /*
     * @return Mountpoint path.
     */
    boost::filesystem::path getMountpoint() const;

    /*
     * @return List of space names to mount.
     */
    std::vector<std::string> getSpaceNames() const;

    /*
     * @return List of space ids to mount.
     */
    std::vector<std::string> getSpaceIds() const;

    /*
     * @return FUSE mounting options.
     */
    std::vector<std::string> getFuseOpts() const;

    /*
     * @param programName Name of the client program.
     * @return Constructed FUSE arguments from FUSE mount options.
     */
    struct fuse_args getFuseArgs(const char *programName) const;

    std::vector<std::pair<std::string, std::string>> toKeyValueList() const;

private:
    template <typename T> std::shared_ptr<TypedOption<T>> add()
    {
        auto option = std::make_shared<TypedOption<T>>();
        m_options.push_back(option);
        return option;
    }

    template <typename T>
    boost::optional<T> get(const std::vector<std::string> &names) const
    {
        for (const auto &name : names) {
            if (m_vm.count(name) && !m_vm.at(name).defaulted()) {
                return m_vm.at(name).as<T>();
            }
        }
        if (!names.empty() && m_vm.count(names[0])) {
            return m_vm.at(names[0]).as<T>();
        }
        return {};
    }

    std::vector<std::tuple<std::string, std::string, std::string>>
    getOverrideParams() const
    {
        if (m_vm.count("override") && !m_vm.at("override").defaulted()) {
            auto overrideParams =
                m_vm["override"].as<std::vector<std::string>>();

            std::vector<std::tuple<std::string, std::string, std::string>>
                result;

            for (const auto &param : overrideParams) {
                std::vector<std::string> keyValue;
                boost::split(keyValue, param, boost::is_any_of(":"));

                if (keyValue.size() < 3) {
                    LOG(ERROR) << "Invalid command line argument: --override "
                               << param;
                    LOG(ERROR) << "Helper override parameters must have values "
                                  "in the form "
                                  "<storageId>:<parameter>:<value>";
                }

                result.emplace_back(keyValue[0], keyValue[1],
                    boost::algorithm::join(
                        std::vector<std::string>(
                            std::next(keyValue.begin(), 2), keyValue.end()),
                        ":"));
            }

            return result;
        }
        return {};
    }

    boost::optional<std::pair<std::string, std::string>> parseKeyValuePair(
        const std::string &val) const
    {
        std::vector<std::string> keyValue;
        boost::split(keyValue, val, boost::is_any_of(":"));

        if (keyValue.size() != 2) {
            LOG(ERROR) << "Key-value arguments must have values in the form "
                          "<name>:<value>";
            return {};
        }

        return {std::make_pair<std::string, std::string>(
            std::move(keyValue[0]), std::move(keyValue[1]))};
    }

    void selectCommandLine(boost::program_options::options_description &desc,
        const OptionGroup &group) const;

    std::vector<std::string> selectDeprecated() const;

    messages::handshake::ClientType m_clientType;
    boost::program_options::variables_map m_vm;
    boost::filesystem::path m_defaultConfigFilePath;
    boost::filesystem::path m_defaultLogDirPath;
    bool m_emptyArgumentsList = false;
    std::vector<std::string> m_deprecatedEnvs;
    std::vector<std::shared_ptr<Option>> m_options;
};

template <>
inline boost::optional<std::pair<std::string, std::string>>
Options::get<std::pair<std::string, std::string>>(
    const std::vector<std::string> &names) const
{
    for (const auto &name : names) {
        if (m_vm.count(name) && !m_vm.at(name).defaulted()) {
            return parseKeyValuePair(m_vm.at(name).as<std::string>());
        }
    }
    if (!names.empty() && m_vm.count(names[0])) {
        return parseKeyValuePair(m_vm.at(names[0]).as<std::string>());
    }
    return {};
}

} // namespace options
} // namespace client
} // namespace one

#endif // ONECLIENT_OPTIONS_H
