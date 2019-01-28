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

    add<std::vector<std::string>>()
        ->withEnvName("space")
        .withLongName("space")
        .withConfigName("space")
        .withValueName("<name>")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Allows to specify which space should be mounted, "
                         "where the value of the argument is space name. "
                         "Specify multiple times for multiple spaces. If not "
                         "specified, all users spaces will be mounted.");

    add<std::vector<std::string>>()
        ->withEnvName("space_id")
        .withLongName("space-id")
        .withConfigName("space_id")
        .withValueName("<id>")
        .withGroup(OptionGroup::GENERAL)
        .withDescription(
            "Allows to specify which space should be mounted, where the value "
            "of the argument is space id. Specify multiple times for multiple "
            "spaces. If not specified, all users spaces will be mounted.");

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
        .withLongName("io-trace-log")
        .withConfigName("io-trace-log")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Enable detailed IO trace log (experimental).");

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
        .withLongName("no-fullblock-read")
        .withConfigName("no_fullblock_read")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Disable fullblock read mode. With this option read can return "
            "less "
            "data than requested in case it is immediately available and "
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
                         "output data blocks of a single opened file handle.");

    add<unsigned int>()
        ->withLongName("read-buffers-total-size")
        .withConfigName("read_buffers_total_size")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_READ_BUFFERS_TOTAL_SIZE,
            std::to_string(DEFAULT_READ_BUFFERS_TOTAL_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Specify total maximum size in bytes of in-memory cache for "
            "input data blocks of all opened file handles. When 0, read "
            "buffers are unlimited.");

    add<unsigned int>()
        ->withLongName("write-buffers-total-size")
        .withConfigName("write_buffers_total_size")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_WRITE_BUFFERS_TOTAL_SIZE,
            std::to_string(DEFAULT_WRITE_BUFFERS_TOTAL_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Specify total maximum size in bytes of in-memory cache for "
            "output data blocks of all opened file handles. When 0, write "
            "buffers are unlimited.");

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
        ->withLongName("rndrd-prefetch-eval-frequency")
        .withConfigName("rndrd_prefetch_eval_frequency")
        .withValueName("<count>")
        .withDefaultValue(DEFAULT_PREFETCH_EVALUATE_FREQUENCY,
            std::to_string(DEFAULT_PREFETCH_EVALUATE_FREQUENCY))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Number of reads from single file handle which will "
                         "be skipped before next evaluation of cluster "
                         "prefetch. 0 means that prefetch evaluation will be "
                         "performed on each read. (experimental).");

    add<unsigned int>()
        ->withLongName("rndrd-prefetch-block-threshold")
        .withConfigName("rndrd_prefetch_block_threshold")
        .withValueName("<count>")
        .withDefaultValue(0, std::to_string(0))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Number of separate blocks after which replication "
                         "for the file is triggered automatically. 0 disables "
                         "this feature (experimental).");

    add<int>()
        ->withLongName("rndrd-prefetch-cluster-window")
        .withConfigName("rndrd_prefetch_cluster_window")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_PREFETCH_CLUSTER_WINDOW_SIZE,
            std::to_string(DEFAULT_PREFETCH_CLUSTER_WINDOW_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Cluster window size for prefetching in "
                         "[bytes]. When -1 is provided, the "
                         "entire file is considered for "
                         "prefetching (experimental).");

    add<unsigned int>()
        ->withLongName("rndrd-prefetch-cluster-block-threshold")
        .withConfigName("rndrd_prefetch_cluster_block_threshold")
        .withValueName("<count>")
        .withDefaultValue(DEFAULT_PREFETCH_CLUSTER_BLOCK_THRESHOLD,
            std::to_string(DEFAULT_PREFETCH_CLUSTER_BLOCK_THRESHOLD))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Number of separate blocks in a cluster window around "
                         "current read, after which replication of a cluster "
                         "block (window) is triggered (experimental).");

    add<double>()
        ->withLongName("rndrd-prefetch-cluster-window-grow-factor")
        .withConfigName("rndrd_prefetch_cluster_window_grow_factor")
        .withValueName("<fraction>")
        .withDefaultValue(0.0, std::to_string(0.0))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Prefetch cluster window grow factor, which enables "
            "the prefetch window to grow proportionally to "
            "current replication progress - "
            "initial_window_size*[1+grow_factor*file_size*replication_progress/"
            "initial_window_size)] (experimental).");

    add<std::string>()
        ->withLongName("prefetch-mode")
        .withConfigName("prefetch_mode")
        .withImplicitValue(DEFAULT_PREFETCH_MODE)
        .withDefaultValue(DEFAULT_PREFETCH_MODE, DEFAULT_PREFETCH_MODE)
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Defines the type of block prefetch mode. Possible "
                         "values are: async, sync. Default is: async "
                         "(experimental).");

    add<bool>()
        ->asSwitch()
        .withLongName("cluster-prefetch-threshold-random")
        .withConfigName("cluster_prefetch_threshold_random")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Enables random cluster prefetch threshold selection "
                         "(experimental).");

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

    add<std::string>()
        ->withEnvName("tag_on_create")
        .withLongName("tag-on-create")
        .withConfigName("tag_on_create")
        .withValueName("<name>:<value>")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Adds <name>=<value> extended attribute to each "
                         "locally created file.");

    add<std::string>()
        ->withEnvName("tag_on_modify")
        .withLongName("tag-on-modify")
        .withConfigName("tag_on_modify")
        .withValueName("<name>:<value>")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Adds <name>=<value> extended attribute to each "
                         "locally modified file.");

    add<std::vector<std::string>>()
        ->withEnvName("override")
        .withShortName("r")
        .withLongName("override")
        .withConfigName("override")
        .withValueName("<storageId>:<name>:<value>")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Allows to override selected helper parameters for specific "
            "storage, e.g. "
            "'d40f2f63433da7c845886f6fe970048b:mountPoint:/mnt/nfs'");

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
        .withDefaultValue(DEFAULT_MONITORING_PERIOD_SECONDS,
            std::to_string(DEFAULT_MONITORING_PERIOD_SECONDS))
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

    add<bool>()
        ->asSwitch()
        .withLongName("force-fullblock-read")
        .withEnvName("force_fullblock_read")
        .withConfigName("force_fullblock_read")
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

bool Options::isIOTraceLoggerEnabled() const
{
    return get<bool>({"io-trace-log", "io_trace_log"}).get_value_or(false);
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

bool Options::isFullblockReadEnabled() const
{
    return !get<bool>({"no-fullblock-read", "no_fullblock_read"})
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

unsigned int Options::getReadBuffersTotalSize() const
{
    return get<unsigned int>(
        {"read-buffers-total-size", "read_buffers_total_size"})
        .get_value_or(DEFAULT_READ_BUFFERS_TOTAL_SIZE);
}

unsigned int Options::getWriteBuffersTotalSize() const
{
    return get<unsigned int>(
        {"write-buffers-total-size", "write_buffers_total_size"})
        .get_value_or(DEFAULT_WRITE_BUFFERS_TOTAL_SIZE);
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

std::string Options::getPrefetchMode() const
{
    return get<std::string>({"prefetch-mode", "prefetch_mode"})
        .get_value_or("async");
}

unsigned int Options::getRandomReadPrefetchEvaluationFrequency() const
{
    return get<unsigned int>(
        {"rndrd-prefetch-eval-frequency", "rndrd-prefetch-eval-frequency"})
        .get_value_or(DEFAULT_PREFETCH_EVALUATE_FREQUENCY);
}

bool Options::isClusterPrefetchThresholdRandom() const
{
    return get<bool>({"cluster-prefetch-threshold-random",
                         "cluster_prefetch_threshold_random"})
        .get_value_or(false);
}

unsigned int Options::getRandomReadPrefetchBlockThreshold() const
{
    return get<unsigned int>(
        {"rndrd-prefetch-block-threshold", "rndrd_prefetch_block_threshold"})
        .get_value_or(0);
}

int Options::getRandomReadPrefetchClusterWindow() const
{
    return get<int>(
        {"rndrd-prefetch-cluster-window", "rndrd_prefetch_cluster_window"})
        .get_value_or(DEFAULT_PREFETCH_CLUSTER_WINDOW_SIZE);
}

unsigned int Options::getRandomReadPrefetchClusterBlockThreshold() const
{
    return get<unsigned int>({"rndrd-prefetch-cluster-block-threshold",
                                 "rndrd_prefetch_cluster_block_threshold"})
        .get_value_or(DEFAULT_PREFETCH_CLUSTER_BLOCK_THRESHOLD);
}

double Options::getRandomReadPrefetchClusterWindowGrowFactor() const
{
    return get<double>({"rndrd-prefetch-cluster-window-grow-factor",
                           "rndrd_prefetch_cluster_window_grow_factor"})
        .get_value_or(0.0);
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

boost::optional<std::pair<std::string, std::string>>
Options::getOnModifyTag() const
{
    return get<std::pair<std::string, std::string>>(
        {"tag-on-modify", "tag_on_modify"});
}

boost::optional<std::pair<std::string, std::string>>
Options::getOnCreateTag() const
{
    return get<std::pair<std::string, std::string>>(
        {"tag-on-create", "tag_on_create"});
}

std::map<folly::fbstring, std::unordered_map<folly::fbstring, folly::fbstring>>
Options::getHelperOverrideParams() const
{
    std::map<folly::fbstring,
        std::unordered_map<folly::fbstring, folly::fbstring>>
        result;

    for (const auto &p : getOverrideParams()) {
        result[std::get<0>(p)][std::get<1>(p)] = std::get<2>(p);
    }

    return result;
}

std::unordered_map<folly::fbstring, folly::fbstring>
Options::getHelperOverrideParams(const folly::fbstring &storageId) const
{
    const auto &overrideParams = getHelperOverrideParams();

    if (overrideParams.find(storageId) != overrideParams.cend())
        return overrideParams.at(storageId);

    return {};
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
        .get_value_or(DEFAULT_MONITORING_PERIOD_SECONDS);
}

boost::filesystem::path Options::getMountpoint() const
{
    return get<boost::filesystem::path>({"mountpoint"}).get();
}

std::vector<std::string> Options::getSpaceNames() const
{
    return get<std::vector<std::string>>({"space"}).get_value_or({});
}

std::vector<std::string> Options::getSpaceIds() const
{
    return get<std::vector<std::string>>({"space-id"}).get_value_or({});
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
