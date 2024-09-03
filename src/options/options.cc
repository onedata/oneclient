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
#include <thread>
#include <vector>

namespace one {

namespace client {
namespace options {

Options::Options(messages::handshake::ClientType clientType)
    : m_clientType{clientType}
    , m_defaultConfigFilePath{boost::filesystem::path(ONECLIENT_CONFIG_DIR) /
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

    add<bool>()
        ->asSwitch()
        .withLongName("ignore-env")
        .withConfigName("ignore_env")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Ignore options from environment variables.");

    add<std::string>()
        ->withShortName("H")
        .withLongName("host")
        .withEnvName("provider_host")
        .withConfigName("provider_host")
        .withValueName("<host>")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify the hostname of the Oneprovider instance to "
                         "which the Oneclient should connect.");

    if (m_clientType == messages::handshake::ClientType::ones3) {
        add<std::string>()
            ->withShortName("Z")
            .withLongName("onezone-host")
            .withEnvName("onezone_host")
            .withConfigName("onezone_host")
            .withValueName("<onezone_host>")
            .withGroup(OptionGroup::GENERAL)
            .withDescription("Specify the hostname of the Onezone instance to "
                             "which the OneS3 should connect.");
    }

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

    add<boost::filesystem::path>()
        ->withEnvName("custom_ca_dir")
        .withLongName("custom-ca-dir")
        .withConfigName("custom_ca_dir")
        .withValueName("<path>")
        .withGroup(OptionGroup::GENERAL)
        .withDescription(
            "Path to directory with custom CA certificates in PEM format.");

    add<bool>()
        ->asSwitch()
        .withLongName("io-trace-log")
        .withConfigName("io_trace_log")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Enable detailed IO trace log (experimental).");

    add<bool>()
        ->asSwitch()
        .withLongName("message-trace-log")
        .withConfigName("message_trace_log")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::INVISIBLE)
        .withDescription("Enable detailed ProtoBuf messages trace log.");

    add<bool>()
        ->asSwitch()
        .withLongName("log-read-write-perf")
        .withConfigName("log_read_write_perf")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Enable read write performance logger.");

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
        .withGroup(OptionGroup::INVISIBLE)
        .withDescription(
            "Specify number of parallel communicator threads (deprecated).");

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

    add<bool>()
        ->asSwitch()
        .withLongName("no-xattr")
        .withConfigName("no_xattr")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Disable extended attributes support.");

    add<unsigned int>()
        ->withLongName("provider-timeout")
        .withConfigName("provider_timeout")
        .withValueName("<duration>")
        .withDefaultValue(
            DEFAULT_PROVIDER_TIMEOUT, std::to_string(DEFAULT_PROVIDER_TIMEOUT))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify Oneprovider connection timeout in seconds.");

    add<unsigned int>()
        ->withLongName("storage-timeout")
        .withConfigName("storage_timeout")
        .withValueName("<duration>")
        .withDefaultValue(
            DEFAULT_STORAGE_TIMEOUT, std::to_string(DEFAULT_STORAGE_TIMEOUT))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify I/O storage timeout in seconds.");

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

    add<unsigned int>()
        ->withLongName("min-block-prefetch-size")
        .withConfigName("min_block_prefetch_size")
        .withValueName("<bytes>")
        .withDefaultValue(DEFAULT_MIN_BLOCK_PREFETCH_SIZE,
            std::to_string(DEFAULT_MIN_BLOCK_PREFETCH_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify the minimum prefetch block size.");

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
        .withDescription(
            "Maximum number of file attributes cached in the metadata cache.");

    add<unsigned int>()
        ->withLongName("readdir-prefetch-size")
        .withConfigName("readdir_prefetch_size")
        .withValueName("<size>")
        .withDefaultValue(DEFAULT_READDIR_PREFETCH_SIZE,
            std::to_string(DEFAULT_READDIR_PREFETCH_SIZE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify the size of requests made during readdir "
                         "prefetch (in number of dir entries).");

    add<unsigned int>()
        ->withLongName("dir-cache-drop-after")
        .withConfigName("dir_cache_drop_after")
        .withValueName("<seconds>")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Specify (in seconds) how long should directories be "
                         "cached since last activity. When 0 is provided, "
                         "the cache never expires.");

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

    add<uint64_t>()
        ->withLongName("emulate-available-space")
        .withConfigName("emulate_available_space")
        .withValueName("<bytes>")
        .withDefaultValue(DEFAULT_EMULATE_AVAILABLE_SPACE,
            std::to_string(DEFAULT_EMULATE_AVAILABLE_SPACE))
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "When set to non-zero value, emulates available space reported by "
            "stat system call to specified number of bytes.");

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
        .withLongName("disable-log-buffering")
        .withEnvName("disable_log_buffering")
        .withConfigName("disable_log_buffering")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Disable log buffering.");

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

    add<bool>()
        ->asSwitch()
        .withLongName("only-full-replicas")
        .withImplicitValue(true)
        .withGroup(OptionGroup::INVISIBLE)
        .withDescription("Show only fully replicated files.");

    add<bool>()
        ->asSwitch()
        .withLongName("hard-link-count")
        .withImplicitValue(true)
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Show hard link count properly in stat.");

    add<bool>()
        ->asSwitch()
        .withLongName("enable-archivematica")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Enable Archivematica mode.");

    add<bool>()
        ->asSwitch()
        .withLongName("open-shares-mode")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription("Enable open share mode, in which space directories "
                         "list open data shares.");

    add<bool>()
        ->asSwitch()
        .withLongName("show-space-ids")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::ADVANCED)
        .withDescription(
            "Show space Id's instead of space names in the filesystem tree.");

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

    if (m_clientType == messages::handshake::ClientType::ones3) {
        add<std::string>()
            ->withEnvName("ones3_support_admin_group")
            .withLongName("ones3-support-admin-group")
            .withConfigName("ones3_support_admin_group")
            .withValueName("<support_admin_group>")
            .withGroup(OptionGroup::ONES3)
            .withDescription("Specifies a Onedata group which will be added to "
                             "each create and supported space.");

        add<std::string>()
            ->withEnvName("ones3_support_storage_id")
            .withLongName("ones3-support-storage-id")
            .withConfigName("ones3_support_storage_id")
            .withValueName("<storage_id>")
            .withGroup(OptionGroup::ONES3)
            .withDescription(
                "Specifies default storage id for supporting new buckets.");

        add<size_t>()
            ->withEnvName("ones3_support_storage_size")
            .withLongName("ones3-support-storage-size")
            .withConfigName("ones3_support_storage_size")
            .withValueName("<support_storage_size>")
            .withDefaultValue(DEFAULT_ONES3_STORAGE_SUPPORT_SIZE, "")
            .withGroup(OptionGroup::ONES3)
            .withDescription("Specifies default storage support size for new "
                             "buckets in bytes.");

        add<std::string>()
            ->withEnvName("ones3_support_storage_credentials")
            .withLongName("ones3-support-storage-credentials")
            .withConfigName("ones3_support_storage_credentials")
            .withValueName("<support_storage_credentials>")
            .withGroup(OptionGroup::ONES3)
            .withDescription("Specifies credentials needed to automatically "
                             "support storage for a new bucket.");

        add<unsigned int>()
            ->withEnvName("ones3_http_port")
            .withLongName("ones3-http-port")
            .withConfigName("ones3_http_port")
            .withValueName("<port>")
            .withGroup(OptionGroup::ONES3)
            .withDescription("HTTP port for OneS3 service, if not provided "
                             "HTTP listener will "
                             "be disabled if HTTPS port is provided.");

        add<unsigned int>()
            ->withEnvName("ones3_https_port")
            .withLongName("ones3-https-port")
            .withConfigName("ones3_https_port")
            .withValueName("<port>")
            .withGroup(OptionGroup::ONES3)
            .withDescription(
                "HTTPS port for OneS3 service - if not provided HTTPS "
                "endpoint will not be enabled.");

        add<std::string>()
            ->withEnvName("ones3_address_bind")
            .withLongName("ones3-address-bind")
            .withConfigName("ones3_address_bind")
            .withValueName("<address>")
            .withDefaultValue("0.0.0.0", "")
            .withGroup(OptionGroup::ONES3)
            .withDescription("Address bind for OneS3 service.");

        add<boost::filesystem::path>()
            ->withEnvName("ones3_ssl_cert")
            .withLongName("ones3-ssl-cert")
            .withConfigName("ones3_ssl_cert")
            .withValueName("<path>")
            .withGroup(OptionGroup::ONES3)
            .withDescription(
                "Path to OneS3 HTTPS endpoint SSL certificate in PEM format.");

        add<boost::filesystem::path>()
            ->withEnvName("ones3_ssl_key")
            .withLongName("ones3-ssl-key")
            .withConfigName("ones3_ssl_key")
            .withValueName("<path>")
            .withGroup(OptionGroup::ONES3)
            .withDescription(
                "Path to OneS3 HTTPS endpoint SSL key in PEM format.");

        add<unsigned int>()
            ->withEnvName("ones3_thread_num")
            .withLongName("ones3-thread-num")
            .withConfigName("ones3_thread_num")
            .withValueName("<port>")
            .withDefaultValue(std::thread::hardware_concurrency(), "")
            .withGroup(OptionGroup::ONES3)
            .withDescription("Number of threads of the OneS3 server.");

        add<bool>()
            ->asSwitch()
            .withEnvName("ones3_disable_bucket_operations")
            .withLongName("ones3-disable-bucket-operations")
            .withConfigName("ones3_disable_bucket_operations")
            .withValueName("<bool>")
            .withGroup(OptionGroup::ONES3)
            .withDescription(
                "Disables bucket creation and deletion operations.");

        add<unsigned int>()
            ->withEnvName("ones3_keepalive_requests")
            .withLongName("ones3-keepalive-requests")
            .withConfigName("ones3_keepalive_requests")
            .withValueName("<num>")
            .withDefaultValue(DEFAULT_ONES3_KEEPALIVE_REQUESTS_MAX, "")
            .withGroup(OptionGroup::ONES3)
            .withDescription(
                "Number of concurrent keepalive requests maintained "
                "by OneS3 server.");

        add<size_t>()
            ->withEnvName("ones3_max_body_size")
            .withLongName("ones3-max-body-size")
            .withConfigName("ones3_max_body_size")
            .withValueName("<bytes>")
            .withDefaultValue(DEFAULT_ONES3_MAX_BODY_SIZE, "")
            .withGroup(OptionGroup::ONES3)
            .withDescription(
                "Maximum size of body accepted by OneS3 requests.");

        add<size_t>()
            ->withEnvName("ones3_max_body_memory_size")
            .withLongName("ones3-max-body-memory-size")
            .withConfigName("ones3_max_body_memory_size")
            .withValueName("<bytes>")
            .withDefaultValue(DEFAULT_ONES3_MAX_BODY_MEMORY_SIZE, "")
            .withGroup(OptionGroup::ONES3)
            .withDescription("Maximum size of a single request body buffered "
                             "by OneS3 server.");

        add<size_t>()
            ->withEnvName("ones3_stream_get_threshold")
            .withLongName("ones3-stream-get-threshold")
            .withConfigName("ones3_stream_get_threshold")
            .withValueName("<bytes>")
            .withDefaultValue(DEFAULT_ONES3_GET_STREAM_THRESHOLD, "")
            .withGroup(OptionGroup::ONES3)
            .withDescription("Minimum GET range size to use streaming instead "
                             "of single read.");

        add<unsigned int>()
            ->withEnvName("ones3_idle_connection_timeout")
            .withLongName("ones3-idle-connection-timeout")
            .withConfigName("ones3_idle_connection_timeout")
            .withValueName("<seconds>")
            .withDefaultValue(DEFAULT_ONES3_IDLE_CONNECTION_TIMEOUT, "")
            .withGroup(OptionGroup::ONES3)
            .withDescription(
                "Time in seconds after which idle connections to OneS3 "
                "server are closed.");

        add<std::string>()
            ->withEnvName("ones3_readiness_probe_auth")
            .withLongName("ones3-readiness-probe-auth")
            .withConfigName("ones3_readiness_probe_auth")
            .withValueName("<readiness_probe_auth>")
            .withGroup(OptionGroup::ONES3)
            .withDescription("Specifies the basic authentication for OneS3 "
                             "readiness probe (default none).");
    }

    if (m_clientType == messages::handshake::ClientType::oneclient) {
        add<boost::filesystem::path>()
            ->positional("mountpoint", 1)
            .required()
            .withEnvName("mountpoint")
            .withConfigName("mountpoint")
            .withValueName("<path>")
            .withGroup(OptionGroup::INVISIBLE)
            .withDescription("Specify path for Oneclient mountpoint.");
    }

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

    if (!isIgnoreEnv())
        parser.parseEnvironment(m_deprecatedEnvs, m_vm);

    if (m_clientType == messages::handshake::ClientType::ones3 &&
        !exists(getConfigFilePath())) {
        fmt::print(stderr, "WARN: Configuration file not found in {}\n",
            getConfigFilePath().c_str());
    }

    parser.parseConfigFile(getConfigFilePath(), m_vm);

    if (m_clientType == messages::handshake::ClientType::ones3) {
        if (!getOnezoneHost()) {
            throw boost::program_options::error_with_no_option_name(
                "ERROR: required option 'onezone-host' missing");
        }
        if (!getProviderHost()) {
            throw boost::program_options::error_with_no_option_name(
                "ERROR: required option 'host' missing");
        }
    }
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

std::string Options::formatHelpOneS3(const char *programName) const
{
    boost::program_options::options_description general{"General options"};
    selectCommandLine(general, OptionGroup::GENERAL);
    boost::program_options::options_description ones3{
        "Onedata S3 server options"};
    selectCommandLine(ones3, OptionGroup::ONES3);
    boost::program_options::options_description advanced{"Advanced options"};
    selectCommandLine(advanced, OptionGroup::ADVANCED);
    boost::program_options::options_description monitoring{
        "Monitoring options"};
    selectCommandLine(monitoring, OptionGroup::MONITORING);

    std::stringstream ss;
    ss << "Usage: " << programName << " [options]\n";
    ss << "\n";
    ss << "OneS3 - a Onedata scalable S3 proxy service.";
    ss << "\n";
    ss << "\n";
    ss << general;
    ss << "\n";
    ss << ones3;
    ss << "\n";
    ss << advanced;
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

bool Options::isIgnoreEnv() const
{
    return get<bool>({"ignore-env", "ignore_env"}).get_value_or(false);
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
    return get<unsigned int>(
        {"verbose-log-level", "verbose_log_level", "verbose_log_level"})
        .get_value_or(0);
}

bool Options::disableLogBuffering() const
{
    return get<bool>({"disable-log-buffering", "disable_log_buffering"})
        .get_value_or(false);
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

boost::optional<std::string> Options::getOnezoneHost() const
{
    return get<std::string>(
        {"onezone-host", "onezone_host", "onezone_hostname"});
}

unsigned int Options::getProviderPort() const
{
    return get<unsigned int>({"port", "provider_port"})
        .get_value_or(DEFAULT_PROVIDER_PORT);
}

boost::optional<boost::filesystem::path>
Options::getCustomCACertificateDir() const
{
    return get<boost::filesystem::path>({"custom-ca-dir", "custom_ca_dir"});
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

bool Options::isMessageTraceLoggerEnabled() const
{
    return get<bool>({"message-trace-log", "message_trace_log"})
        .get_value_or(false);
}

bool Options::isReadWritePerfEnabled() const
{
    return get<bool>({"log-read-write-perf", "log_read_write_perf"})
        .get_value_or(false);
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
    return DEFAULT_COMMUNICATOR_THREAD_COUNT;
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

bool Options::enableExtendedAttributes() const
{
    return !get<bool>({"no-xattr", "no_xattr"}).get_value_or(false);
}

std::chrono::seconds Options::getProviderTimeout() const
{
    return std::chrono::seconds{
        get<unsigned int>({"provider-timeout", "provider_timeout"})
            .get_value_or(DEFAULT_PROVIDER_TIMEOUT)};
}

std::chrono::seconds Options::getStorageTimeout() const
{
    return std::chrono::seconds{
        get<unsigned int>({"storage-timeout", "storage_timeout"})
            .get_value_or(DEFAULT_STORAGE_TIMEOUT)};
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

unsigned int Options::getMinimumBlockPrefetchSize() const
{
    return get<unsigned int>(
        {"min-block-prefetch-size", "min_block_prefetch_size"})
        .get_value_or(DEFAULT_MIN_BLOCK_PREFETCH_SIZE);
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

std::chrono::seconds Options::getDirectoryCacheDropAfter() const
{
    auto defaultDirCacheDropAfter = DEFAULT_DIR_CACHE_DROP_AFTER;
    if (get<unsigned int>({"dir-cache-drop-after", "dir_cache_drop_after"}) ==
            boost::none &&
        isOpenSharesModeEnabled()) {
        defaultDirCacheDropAfter =
            DEFAULT_DIR_CACHE_DROP_AFTER_IN_OPEN_SHARE_MODE;
    }

    return std::chrono::seconds{
        get<unsigned int>({"dir-cache-drop-after", "dir_cache_drop_after"})
            .get_value_or(defaultDirCacheDropAfter)};
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

uint64_t Options::getEmulateAvailableSpace() const
{
    return get<uint64_t>({"emulate-available-space", "emulate_available_space"})
        .get_value_or(DEFAULT_EMULATE_AVAILABLE_SPACE);
}

bool Options::showOnlyFullReplicas() const
{
    return get<bool>({"only-full-replicas", "only_full_replicas"})
        .get_value_or(false);
}

bool Options::showHardLinkCount() const
{
    return get<bool>({"hard-link-count", "hard_link_count"})
        .get_value_or(false);
}

bool Options::isArchivematicaModeEnabled() const
{
    return get<bool>({"enable-archivematica", "enable_archivematica"})
        .get_value_or(false);
}

bool Options::isOpenSharesModeEnabled() const
{
    return get<bool>({"open-shares-mode", "open-share-mode"})
        .get_value_or(false);
}

bool Options::showSpaceIds() const
{
    return get<bool>({"show-space-ids", "show-space-ids"}).get_value_or(false);
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
    return get<std::string>({"graphite-url", "graphite_url", "graphite_url"});
}

boost::optional<std::string>
Options::getMonitoringGraphiteNamespacePrefix() const
{
    return get<std::string>({"graphite-namespace-prefix",
        "graphite_namespace_prefix", "graphite_namespace_prefix"});
}

unsigned int Options::getMonitoringReportingPeriod() const
{
    return get<unsigned int>(
        {"monitoring-period", "monitoring_period", "monitoring_period"})
        .get_value_or(DEFAULT_MONITORING_PERIOD_SECONDS);
}

boost::optional<std::string> Options::getOneS3SupportAdminGroupId() const
{
    return get<std::string>({"ones3-support-admin-group",
        "ones3_support_admin_group", "ones3_support_admin_group"});
}

size_t Options::getOneS3SupportStorageSize() const
{
    return get<size_t>(
        {"ones3-support-storage-size", "ones3_support_storage_size",
            "ones3_support_storage_size"})
        .get_value_or(DEFAULT_ONES3_STORAGE_SUPPORT_SIZE);
}

boost::optional<std::string> Options::getOneS3SupportStorageCredentials() const
{
    return get<std::string>({"ones3-support-storage-credentials",
        "ones3_support_storage_credentials",
        "ones3_support_storage_credentials"});
}

boost::optional<std::string> Options::getOneS3SupportStorageId() const
{
    return get<std::string>({"ones3-support-storage-id",
        "ones3_support_storage_id", "ones3_support_storage_id"});
}

bool Options::areOneS3BucketOperationsDisabled() const
{
    return get<bool>(
        {"ones3-disable-bucket-operations", "ones3_disable_bucket_operations",
            "ones3_disable_bucket_operations"})
        .get_value_or(false);
}

boost::optional<unsigned int> Options::getOneS3HTTPPort() const
{
    return get<unsigned int>(
        {"ones3-http-port", "ones3_http_port", "ones3_http_port"});
}

boost::optional<unsigned int> Options::getOneS3HTTPSPort() const
{
    return get<unsigned int>(
        {"ones3-https-port", "ones3_https_port", "ones3_https_port"});
}

boost::optional<boost::filesystem::path>
Options::getOneS3SSLCertificatePath() const
{
    return get<boost::filesystem::path>(
        {"ones3-ssl-cert", "ones3_ssl_cert", "ones3_ssl_cert"});
}

boost::optional<boost::filesystem::path> Options::getOneS3SSLKeyPath() const
{
    return get<boost::filesystem::path>(
        {"ones3-ssl-key", "ones3_ssl_key", "ones3_ssl_key"});
}

boost::optional<std::string> Options::getOneS3ReadinessProbeBasicAuth() const
{
    return get<std::string>({"ones3-readiness-probe-auth",
        "ones3_readiness_probe_auth", "ones3_readiness_probe_auth"});
}

unsigned int Options::getOneS3ThreadNum() const
{
    return get<unsigned int>(
        {"ones3-thread-num", "ones3_thread_num", "ones3_thread_num"})
        .get_value_or(std::thread::hardware_concurrency());
}

unsigned int Options::getOneS3KeepaliveRequests() const
{
    return get<unsigned int>(
        {"ones3-keepalive-requests", "ones3_keepalive_requests",
            "ones3_keepalive_requests"})
        .get_value_or(DEFAULT_ONES3_KEEPALIVE_REQUESTS_MAX);
}

size_t Options::getOneS3MaxBodySize() const
{
    return get<size_t>(
        {"ones3-max-body-size", "ones3_max_body_size", "ones3_max_body_size"})
        .get_value_or(DEFAULT_ONES3_MAX_BODY_SIZE);
}

size_t Options::getOneS3MaxBodyMemorySize() const
{
    return get<size_t>(
        {"ones3-max-memory-body-size", "ones3_max_memory_body_size",
            "ones3_max_memory_body_size"})
        .get_value_or(DEFAULT_ONES3_MAX_BODY_MEMORY_SIZE);
}

size_t Options::getOneS3StreamGetThreshold() const
{
    return get<size_t>(
        {"ones3-stream-get-threshold", "ones3_stream_get_threshold",
            "ones3_stream_get_threshold"})
        .get_value_or(DEFAULT_ONES3_GET_STREAM_THRESHOLD);
}

unsigned int Options::getOneS3IdleConnectionTimeout() const
{
    return get<unsigned int>(
        {"ones3-idle-connection-timeout", "ones3_idle_connection_timeout",
            "ones3_idle_connection_timeout"})
        .get_value_or(DEFAULT_ONES3_IDLE_CONNECTION_TIMEOUT);
}

std::string Options::getOneS3AddressBind() const
{
    return get<std::string>(
        {"ones3-address-bind", "ones3_address_bind", "ones3_address_bind"})
        .get_value_or("0.0.0.0");
}

int Options::getOneS3FileMode() const
{
    auto val = get<std::string>(
        {"ones3-file-mode", "ones3_file_mode", "ones3_file_mode"})
                   .get_value_or(DEFAULT_ONES3_FILE_MODE);

    return std::stoi(val, nullptr, 8);
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
#if FUSE_USE_VERSION <= 30
    fuse_opt_add_arg(&args, "-obig_writes");
#endif

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

std::vector<std::pair<std::string, std::string>> Options::toKeyValueList() const
{
    std::vector<std::pair<std::string, std::string>> result;

    for (const auto &it : m_vm) {
        auto name = it.first;

        if (name == "token")
            continue;

        if (it.second.defaulted())
            continue;

        const auto &value = it.second.value();

        if (value.type() == typeid(std::string)) {
            result.emplace_back(
                std::move(name), boost::any_cast<std::string>(value));
        }
        else if (value.type() == typeid(boost::filesystem::path)) {
            result.emplace_back(std::move(name),
                boost::any_cast<boost::filesystem::path>(value).string());
        }
        else if (value.type() == typeid(bool)) {
            result.emplace_back(std::move(name),
                boost::any_cast<bool>(value) ? "true" : "false");
        }
        else if (value.type() == typeid(double)) {
            result.emplace_back(std::move(name),
                std::to_string(boost::any_cast<double>(value)));
        }
        else if (value.type() == typeid(int)) {
            result.emplace_back(
                std::move(name), std::to_string(boost::any_cast<int>(value)));
        }
        else if (value.type() == typeid(unsigned int)) {
            result.emplace_back(std::move(name),
                std::to_string(boost::any_cast<unsigned int>(value)));
        }
        else if (value.type() == typeid(uint64_t)) {
            result.emplace_back(std::move(name),
                std::to_string(boost::any_cast<uint64_t>(value)));
        }
        else if (value.type() == typeid(std::vector<std::string>)) {
            // Handle multiple value command line options
            for (const auto &v :
                boost::any_cast<std::vector<std::string>>(value)) {
                result.emplace_back(name, v);
            }
        }
    }

    return result;
}

} // namespace options
} // namespace client
} // namespace one
