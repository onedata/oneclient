/**
 * @file options.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "options.h"
#include "option.h"
#include "optionsParser.h"
#include "version.h"

#include <boost/program_options.hpp>

#include <iostream>
#include <sstream>
#include <vector>

namespace one {
namespace client {
namespace options {

Options::Options()
    : m_defaultConfigFilePath{boost::filesystem::path(ONECLIENT_CONFIG_DIR) /
          boost::filesystem::path(CONFIG_FILE_NAME)}
    , m_defaultLogDirPath{"/tmp/oneclient/" + std::to_string(geteuid())}
{
    add<bool>()
        ->asSwitch()
        .withShortName("h")
        .withLongName("help")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Show this help and exit.");

    add<bool>()
        ->asSwitch()
        .withShortName("V")
        .withLongName("version")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Show current Oneclient version and exit.");

    add<bool>()
        ->asSwitch()
        .withShortName("u")
        .withLongName("unmount")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Unmount Oneclient and exit.");

    add<boost::filesystem::path>()
        ->withShortName("c")
        .withLongName("config")
        .withEnvName("config")
        .withValueName("<path>")
        .withDefaultValue(
            m_defaultConfigFilePath, m_defaultConfigFilePath.c_str())
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify path to config file.");

    add<std::string>()
        ->withShortName("H")
        .withLongName("host")
        .withEnvName("provider_host")
        .withConfigName("provider_host")
        .withValueName("<host>")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify the hostname of the Oneprovider instance to "
                         "which the Oneclient should connect.");

    add<unsigned int>()
        ->withShortName("P")
        .withLongName("port")
        .withEnvName("provider_port")
        .withConfigName("provider_port")
        .withValueName("<port>")
        .withDefaultValue(
            DEFAULT_PROVIDER_PORT, std::to_string(DEFAULT_PROVIDER_PORT))
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify the port to which the Oneclient should "
                         "connect on the Oneprovider.");

    add<bool>()
        ->asSwitch()
        .withShortName("i")
        .withEnvName("insecure")
        .withLongName("insecure")
        .withConfigName("insecure")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Disable verification of server certificate, allows "
                         "to connect to servers without valid certificate.");

    add<std::string>()
        ->withShortName("t")
        .withLongName("token")
        .withEnvName("access_token")
        .withConfigName("access_token")
        .withValueName("<token>")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify Onedata access token for authentication and "
                         "authorization.");

    add<boost::filesystem::path>()
        ->withShortName("l")
        .withLongName("log-dir")
        .withEnvName("log_dir")
        .withConfigName("log_dir")
        .withValueName("<path>")
        .withDefaultValue(m_defaultLogDirPath, m_defaultLogDirPath.c_str())
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify custom path for Oneclient logs.");

    add<bool>()
        ->asSwitch()
        .withLongName("force-proxy-io")
        .withConfigName("force_proxy_io")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Force proxied access to storage via Oneprovider for all spaces.");

    add<bool>()
        ->asSwitch()
        .withLongName("force-direct-io")
        .withConfigName("force_direct_io")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Force direct access to storage for all spaces.");

    add<unsigned int>()
        ->withLongName("buffer-scheduler-thread-count")
        .withConfigName("buffer_scheduler_thread_count")
        .withValueName("<threads>")
        .withDefaultValue(DEFAULT_BUFFER_SCHEDULER_THREAD_COUNT,
            std::to_string(DEFAULT_BUFFER_SCHEDULER_THREAD_COUNT))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Specify number of parallel buffer scheduler threads.");

    add<unsigned int>()
        ->withLongName("communicator-pool-size")
        .withConfigName("communicator_pool_size")
        .withValueName("<connections>")
        .withDefaultValue(DEFAULT_COMMUNICATOR_POOL_SIZE,
            std::to_string(DEFAULT_COMMUNICATOR_POOL_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify number of connections in communicator pool.");

    add<unsigned int>()
        ->withLongName("communicator-thread-count")
        .withConfigName("communicator_thread_count")
        .withValueName("<threads>")
        .withDefaultValue(DEFAULT_COMMUNICATOR_THREAD_COUNT,
            std::to_string(DEFAULT_COMMUNICATOR_THREAD_COUNT))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify number of parallel communicator threads.");

    add<unsigned int>()
        ->withLongName("scheduler-thread-count")
        .withConfigName("scheduler_thread_count")
        .withValueName("<threads>")
        .withDefaultValue(DEFAULT_SCHEDULER_THREAD_COUNT,
            std::to_string(DEFAULT_SCHEDULER_THREAD_COUNT))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify number of parallel scheduler threads.");

    add<unsigned int>()
        ->withLongName("storage-helper-thread-count")
        .withConfigName("storage_helper_thread_count")
        .withValueName("<threads>")
        .withDefaultValue(DEFAULT_STORAGE_HELPER_THREAD_COUNT,
            std::to_string(DEFAULT_STORAGE_HELPER_THREAD_COUNT))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify number of parallel storage helper threads.");

    add<bool>()
        ->asSwitch()
        .withLongName("no-buffer")
        .withConfigName("no_buffer")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Disable in-memory cache for input/output data blocks.");

    add<unsigned int>()
        ->withLongName("provider-timeout")
        .withConfigName("provider_timeout")
        .withValueName("<duration>")
        .withDefaultValue(
            DEFAULT_PROVIDER_TIMEOUT, std::to_string(DEFAULT_PROVIDER_TIMEOUT))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify Oneprovider connection timeout in seconds.");

    add<bool>()
        ->asSwitch()
        .withLongName("disable-read-events")
        .withConfigName("disable_read_events")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Disable reporting of file read events.");

    add<bool>()
        ->asSwitch()
        .withLongName("force-fullblock-read")
        .withConfigName("force_fullblock_read")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Force fullblock read mode. By default read can return less "
            "data than request in case it is immediately available and "
            "consecutive blocks need to be prefetched from remote storage.");

    add<unsigned int>()
        ->withLongName("read-buffer-min-size")
        .withConfigName("read_buffer_min_size")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_READ_BUFFER_MIN_SIZE,
            std::to_string(DEFAULT_READ_BUFFER_MIN_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify minimum size in bytes of in-memory cache for "
                         "input data blocks.");

    add<unsigned int>()
        ->withLongName("read-buffer-max-size")
        .withConfigName("read_buffer_max_size")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_READ_BUFFER_MAX_SIZE,
            std::to_string(DEFAULT_READ_BUFFER_MAX_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify maximum size in bytes of in-memory cache for "
                         "input data blocks.");

    add<unsigned int>()
        ->withLongName("read-buffer-prefetch-duration")
        .withConfigName("read_buffer_prefetch_duration")
        .withValueName("<duration>")
        .withDefaultValue(DEFAULT_READ_BUFFER_PREFETCH_DURATION,
            std::to_string(DEFAULT_READ_BUFFER_PREFETCH_DURATION))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify read ahead period in seconds of in-memory "
                         "cache for input data blocks.");

    add<unsigned int>()
        ->withLongName("write-buffer-min-size")
        .withConfigName("write_buffer_min_size")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_WRITE_BUFFER_MIN_SIZE,
            std::to_string(DEFAULT_WRITE_BUFFER_MIN_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify minimum size in bytes of in-memory cache for "
                         "output data blocks.");

    add<unsigned int>()
        ->withLongName("write-buffer-max-size")
        .withConfigName("write_buffer_max_size")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_WRITE_BUFFER_MAX_SIZE,
            std::to_string(DEFAULT_WRITE_BUFFER_MAX_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify maximum size in bytes of in-memory cache for "
                         "output data blocks.");

    add<unsigned int>()
        ->withLongName("write-buffer-flush-delay")
        .withConfigName("write_buffer_flush_delay")
        .withValueName("<delay>")
        .withDefaultValue(DEFAULT_WRITE_BUFFER_FLUSH_DELAY,
            std::to_string(DEFAULT_WRITE_BUFFER_FLUSH_DELAY))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify idle period in seconds before flush of "
                         "in-memory cache for output data blocks.");

    add<double>()
        ->withLongName("seqrd-prefetch-threshold")
        .withConfigName("seqrd_prefetch_threshold")
        .withValueName("<fraction>")
        .withDefaultValue(1.0, std::to_string(1.0))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify the fraction of the file, which will trigger "
                         "replication prefetch after that part of the file is "
                         "already replicated (experimental).");

    add<double>()
        ->withLongName("rndrd-prefetch-threshold")
        .withConfigName("rndrd_prefetch_threshold")
        .withValueName("<fraction>")
        .withDefaultValue(1.0, std::to_string(1.0))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify the fraction of the file, which will trigger "
                         "replication prefetch after that part of the file is "
                         "already replicated in random blocks across entire "
                         "file (experimental).");

    add<unsigned int>()
        ->withLongName("rndrd-prefetch-block-threshold")
        .withConfigName("rndrd_prefetch_block_threshold")
        .withValueName("<count>")
        .withDefaultValue(0, std::to_string(0))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Number of separate blocks after which replication "
                         "for the file is triggered automatically. 0 disables "
                         "this feature.");

    add<bool>()
        ->asSwitch()
        .withLongName("prefetch-mode-async")
        .withConfigName("prefetch_mode_async")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Enables asynchronous replication requests.");

    add<unsigned int>()
        ->withLongName("metadata-cache-size")
        .withConfigName("metadata_cache_size")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_METADATA_CACHE_SIZE,
            std::to_string(DEFAULT_METADATA_CACHE_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Number of separate blocks after which replication "
                         "for the file is triggered automatically.");

    add<unsigned int>()
        ->withLongName("readdir-prefetch-size")
        .withConfigName("readdir_prefetch_size")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_READDIR_PREFETCH_SIZE,
            std::to_string(DEFAULT_READDIR_PREFETCH_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify the size of requests made during readdir "
                         "prefetch (in number of dir entries).");

    add<bool>()
        ->asSwitch()
        .withShortName("f")
        .withLongName("foreground")
        .withEnvName("fuse_foreground")
        .withConfigName("fuse_foreground")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::FUSE)
        .withDescription("Foreground operation.");

    add<bool>()
        ->asSwitch()
        .withShortName("d")
        .withLongName("debug")
        .withEnvName("fuse_debug")
        .withConfigName("fuse_debug")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::FUSE)
        .withDescription("Enable debug mode (implies -f).");

    add<unsigned int>()
        ->withShortName("v")
        .withLongName("verbose-log-level")
        .withEnvName("verbose_log_level")
        .withConfigName("verbose_log_level")
        .withValueName("<level>")
        .withDefaultValue(0, std::to_string(0))
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify the verbosity level (0-3) for verbose logs "
                         "(only available in debug builds).");

    add<bool>()
        ->asSwitch()
        .withShortName("s")
        .withLongName("single-thread")
        .withEnvName("fuse_single_thread")
        .withConfigName("fuse_single_thread")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::FUSE)
        .withDescription("Single-threaded operation.");

    add<std::vector<std::string>>()
        ->withShortName("o")
        .withLongName("opt")
        .withConfigName("fuse_mount_opt")
        .withValueName("<mount_option>")
        .withGroup(OptionGroup::FUSE)
        .withDescription("Pass mount arguments directly to FUSE.");

    add<std::string>()
        ->withEnvName("monitoring_type")
        .withLongName("monitoring-type")
        .withConfigName("monitoring_type")
        .withValueName("<reporter>")
        .withGroup(OptionGroup::MONITORING)
        .withDescription("Enables performance metrics monitoring - allowed "
                         "values are: graphite.");

    add<bool>()
        ->asSwitch()
        .withEnvName("monitoring_level_basic")
        .withLongName("monitoring-level-basic")
        .withConfigName("monitoring_level_basic")
        .withImplicitValue(true)
        .withDefaultValue(true, "true")
        .withGroup(OptionGroup::MONITORING)
        .withDescription("Sets monitoring reporting level to basic - default.");

    add<bool>()
        ->asSwitch()
        .withEnvName("monitoring_level_full")
        .withLongName("monitoring-level-full")
        .withConfigName("monitoring_level_full")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::MONITORING)
        .withDescription("Sets monitoring reporting level to full.");

    add<unsigned int>()
        ->withEnvName("monitoring_period")
        .withLongName("monitoring-period")
        .withConfigName("monitoring_period")
        .withValueName("<seconds>")
        .withDefaultValue(30, std::to_string(30))
        .withGroup(OptionGroup::MONITORING)
        .withDescription("Performance metrics reporting period.");

    add<std::string>()
        ->withEnvName("graphite_url")
        .withLongName("graphite-url")
        .withConfigName("graphite_url")
        .withValueName("<url>")
        .withGroup(OptionGroup::MONITORING)
        .withDescription("Graphite url - required when monitoring-type is "
                         "'graphite', the scheme can be either tcp or udp and "
                         "default port is 2003");

    add<std::string>()
        ->withEnvName("graphite_namespace_prefix")
        .withLongName("graphite-namespace-prefix")
        .withConfigName("graphite_namespace_prefix")
        .withValueName("<name>")
        .withGroup(OptionGroup::MONITORING)
        .withDescription("Graphite namespace prefix.");

    add<boost::filesystem::path>()
        ->required()
        .positional("mountpoint", 1)
        .withEnvName("mountpoint")
        .withConfigName("mountpoint")
        .withValueName("<path>")
        .withGroup(OptionGroup::INVISIBLE)
        .withDescription("Specify path for Oneclient mountpoint.");

    add<std::string>()
        ->withEnvName("authorization_token")
        .withGroup(OptionGroup::DEPRECATED);

    add<std::string>()
        ->withLongName("authentication")
        .withConfigName("authentication")
        .withGroup(OptionGroup::DEPRECATED);

    add<bool>()
        ->asSwitch()
        .withLongName("no_check_certificate")
        .withEnvName("no_check_certificate")
        .withConfigName("no_check_certificate")
        .withGroup(OptionGroup::DEPRECATED);

    add<std::string>()
        ->withEnvName("provider_hostname")
        .withConfigName("provider_hostname")
        .withGroup(OptionGroup::DEPRECATED);
}

void Options::parse(const int argc, const char *const argv[])
{
    if (argc == 1) {
        m_emptyArgumentsList = true;
        return;
    }

    OptionsParser parser{m_options};
    parser.parseCommandLine(argc, argv, m_vm);

    if (getHelp() || getVersion())
        return;

    parser.parseEnvironment(m_deprecatedEnvs, m_vm);
    parser.parseConfigFile(getConfigFilePath(), m_vm);
    boost::program_options::notify(m_vm);
}

std::string Options::formatDeprecated() const
{
    std::stringstream ss;
    for (const auto &env : m_deprecatedEnvs) {
        ss << "WARNING: use of environment variable '" << env << "' without '"
           << ENVIRONMENT_PREFIX
           << "' prefix is deprecated and will be removed in the future.\n";
    }
    for (const auto &name : selectDeprecated()) {
        ss << "WARNING: option '" << name
           << "' is deprecated and will be removed in the future.\n";
    }
    return ss.str();
}

std::string Options::formatHelp(const char *programName) const
{
    boost::program_options::options_description general{"General options"};
    selectCommandLine(general, OptionGroup::GENERAL);
    boost::program_options::options_description advanced{"Advanced options"};
    selectCommandLine(advanced, OptionGroup::ADVANCED);
    boost::program_options::options_description fuse{"FUSE options"};
    selectCommandLine(fuse, OptionGroup::FUSE);
    boost::program_options::options_description monitoring{
        "Monitoring options"};
    selectCommandLine(monitoring, OptionGroup::MONITORING);

    std::stringstream ss;
    ss << "Usage: " << programName << " [options] mountpoint\n";
    ss << "\n";
    ss << "A Onedata command line client.";
    ss << "\n";
    ss << "\n";
    ss << general;
    ss << "\n";
    ss << advanced;
    ss << "\n";
    ss << fuse;
    ss << "\n";
    ss << monitoring;

    return ss.str();
}

bool Options::hasDeprecated()
{
    return !m_deprecatedEnvs.empty() || !selectDeprecated().empty();
}

bool Options::getHelp() const
{
    return m_emptyArgumentsList || get<bool>({"help"}).get_value_or(false);
}

bool Options::getVersion() const
{
    return get<bool>({"version"}).get_value_or(false);
}

bool Options::getUnmount() const
{
    return get<bool>({"unmount"}).get_value_or(false);
}

bool Options::getForeground() const
{
    return get<bool>({"foreground", "fuse_foreground"}).get_value_or(false);
}

bool Options::getDebug() const
{
    return get<bool>({"debug", "fuse_debug"}).get_value_or(false);
}

unsigned int Options::getVerboseLogLevel() const
{
    return get<unsigned int>({"verbose-log-level"}).get_value_or(0);
}

bool Options::getSingleThread() const
{
    return get<bool>({"single-thread", "fuse_single_thread"})
        .get_value_or(false);
}

boost::optional<std::string> Options::getProviderHost() const
{
    return get<std::string>({"host", "provider_host", "provider_hostname"});
}

unsigned int Options::getProviderPort() const
{
    return get<unsigned int>({"port", "provider_port"})
        .get_value_or(DEFAULT_PROVIDER_PORT);
}

bool Options::isInsecure() const
{
    return get<bool>(
        {"insecure", "no-check-certificate", "no_check_certificate"})
        .get_value_or(false);
}

boost::optional<std::string> Options::getAccessToken() const
{
    return get<std::string>({"token", "access_token", "authorization_token"});
}

boost::filesystem::path Options::getConfigFilePath() const
{
    return get<boost::filesystem::path>({"config"})
        .get_value_or(m_defaultConfigFilePath);
}

boost::filesystem::path Options::getLogDirPath() const
{
    return get<boost::filesystem::path>({"log-dir", "log_dir"})
        .get_value_or(m_defaultLogDirPath);
}

bool Options::isProxyIOForced() const
{
    return get<bool>({"force-proxy-io", "force_proxy_io"}).get_value_or(false);
}

bool Options::isDirectIOForced() const
{
    return get<bool>({"force-direct-io", "force_direct_io"})
        .get_value_or(false);
}

unsigned int Options::getBufferSchedulerThreadCount() const
{
    return get<unsigned int>(
        {"buffer-scheduler-thread-count", "buffer_scheduler_thread_count"})
        .get_value_or(DEFAULT_BUFFER_SCHEDULER_THREAD_COUNT);
}

unsigned int Options::getCommunicatorConnectionPoolSize() const
{
    return get<unsigned int>(
        {"communicator-pool-size", "communicator_pool_size"})
        .get_value_or(DEFAULT_COMMUNICATOR_POOL_SIZE);
}

unsigned int Options::getCommunicatorThreadCount() const
{
    return get<unsigned int>(
        {"communicator-thread-count", "communicator_thread_count"})
        .get_value_or(DEFAULT_COMMUNICATOR_THREAD_COUNT);
}

unsigned int Options::getSchedulerThreadCount() const
{
    return get<unsigned int>(
        {"scheduler-thread-count", "scheduler_thread_count"})
        .get_value_or(DEFAULT_SCHEDULER_THREAD_COUNT);
}

unsigned int Options::getStorageHelperThreadCount() const
{
    return get<unsigned int>(
        {"storage-helper-thread-count", "storage_helper_thread_count"})
        .get_value_or(DEFAULT_STORAGE_HELPER_THREAD_COUNT);
}

bool Options::areFileReadEventsDisabled() const
{
    return get<bool>({"disable-read-events", "disable_read_events"})
        .get_value_or(false);
}

bool Options::isFullblockReadForced() const
{
    return get<bool>({"force-fullblock-read", "force_fullblock_read"})
        .get_value_or(false);
}

bool Options::isIOBuffered() const
{
    return !get<bool>({"no-buffer", "no_buffer"}).get_value_or(false);
}

std::chrono::seconds Options::getProviderTimeout() const
{
    return std::chrono::seconds{
        get<unsigned int>({"provider-timeout", "provider_timeout"})
            .get_value_or(DEFAULT_PROVIDER_TIMEOUT)};
}

unsigned int Options::getReadBufferMinSize() const
{
    return get<unsigned int>({"read-buffer-min-size", "read_buffer_min_size"})
        .get_value_or(DEFAULT_READ_BUFFER_MIN_SIZE);
}

unsigned int Options::getReadBufferMaxSize() const
{
    return get<unsigned int>({"read-buffer-max-size", "read_buffer_max_size"})
        .get_value_or(DEFAULT_READ_BUFFER_MAX_SIZE);
}

std::chrono::seconds Options::getReadBufferPrefetchDuration() const
{
    return std::chrono::seconds{
        get<unsigned int>(
            {"read-buffer-prefetch-duration", "read_buffer_prefetch_duration"})
            .get_value_or(DEFAULT_READ_BUFFER_PREFETCH_DURATION)};
}

unsigned int Options::getWriteBufferMinSize() const
{
    return get<unsigned int>({"write-buffer-min-size", "write_buffer_min_size"})
        .get_value_or(DEFAULT_WRITE_BUFFER_MIN_SIZE);
}

unsigned int Options::getWriteBufferMaxSize() const
{
    return get<unsigned int>({"write-buffer-max-size", "write_buffer_max_size"})
        .get_value_or(DEFAULT_WRITE_BUFFER_MAX_SIZE);
}

std::chrono::seconds Options::getWriteBufferFlushDelay() const
{
    return std::chrono::seconds{
        get<unsigned int>(
            {"write-buffer-flush-delay", "write_buffer_flush_delay"})
            .get_value_or(DEFAULT_WRITE_BUFFER_FLUSH_DELAY)};
}

double Options::getLinearReadPrefetchThreshold() const
{
    return get<double>({"seqrd-prefetch-threshold", "seqrd_prefetch_threshold"})
        .get_value_or(1.0);
}

double Options::getRandomReadPrefetchThreshold() const
{
    return get<double>({"rndrd-prefetch-threshold", "rndrd_prefetch_threshold"})
        .get_value_or(1.0);
}

bool Options::isPrefetchModeAsynchronous() const
{
    return get<bool>({"prefetch-mode-async", "prefetch_mode_async"})
        .get_value_or(false);
}

unsigned int Options::getRandomReadPrefetchBlockThreshold() const
{
    return get<unsigned int>(
        {"rndrd-prefetch-block-threshold", "rndrd_prefetch_block_threshold"})
        .get_value_or(0);
}

unsigned int Options::getMetadataCacheSize() const
{
    return get<unsigned int>({"metadata-cache-size", "metadata_cache_size"})
        .get_value_or(DEFAULT_METADATA_CACHE_SIZE);
}

unsigned int Options::getReaddirPrefetchSize() const
{
    return get<unsigned int>({"readdir-prefetch-size", "readdir_prefetch_size"})
        .get_value_or(DEFAULT_READDIR_PREFETCH_SIZE);
}

bool Options::isMonitoringEnabled() const
{
    return get<std::string>({"monitoring-type", "monitoring_type"})
        .
        operator bool();
}

boost::optional<std::string> Options::getMonitoringType() const
{
    return get<std::string>({"monitoring-type", "monitoring_type"});
}

bool Options::isMonitoringLevelBasic() const
{
    return get<bool>({"monitoring-level-basic", "monitoring_level_basic"})
        .get_value_or(true);
}

bool Options::isMonitoringLevelFull() const
{
    return get<bool>({"monitoring-level-full", "monitoring_level_full"})
        .get_value_or(false);
}

boost::optional<std::string> Options::getMonitoringGraphiteUrl() const
{
    return get<std::string>({"graphite-url", "graphite_url"});
}

boost::optional<std::string>
Options::getMonitoringGraphiteNamespacePrefix() const
{
    return get<std::string>(
        {"graphite-namespace-prefix", "graphite_namespace_prefix"});
}

unsigned int Options::getMonitoringReportingPeriod() const
{
    return get<unsigned int>({"monitoring-period", "monitoring_period"})
        .get_value_or(30);
}

boost::filesystem::path Options::getMountpoint() const
{
    return get<boost::filesystem::path>({"mountpoint"}).get();
}

std::vector<std::string> Options::getFuseOpts() const
{
    return get<std::vector<std::string>>({"opt", "fuse_mount_opt"})
        .get_value_or({});
}

struct fuse_args Options::getFuseArgs(const char *programName) const
{
    struct fuse_args args = FUSE_ARGS_INIT(0, nullptr);

    fuse_opt_add_arg(&args, programName);
    fuse_opt_add_arg(&args, "-obig_writes");

    if (getDebug())
        fuse_opt_add_arg(&args, "-d");
    if (getForeground())
        fuse_opt_add_arg(&args, "-f");
    if (getSingleThread())
        fuse_opt_add_arg(&args, "-s");

    for (const auto &opt : getFuseOpts())
        fuse_opt_add_arg(&args, ("-o" + opt).c_str());

    fuse_opt_add_arg(&args, getMountpoint().c_str());

    return args;
}

void Options::selectCommandLine(
    boost::program_options::options_description &desc,
    const OptionGroup &group) const
{
    for (const auto &option : m_options) {
        if (option->hasGroup(group))
            option->addCommandLine(desc);
    }
}

std::vector<std::string> Options::selectDeprecated() const
{
    std::vector<std::string> deprecated;
    for (const auto &option : m_options) {
        if (option->hasGroup(OptionGroup::DEPRECATED) && option->isSet(m_vm))
            deprecated.push_back(option->name());
    }
    return deprecated;
}

} // namespace options
} // namespace client
} // namespace one
