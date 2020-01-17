/**
 * @file logging.cc
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "logging.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "helpers/init.h"
#include "monitoring/monitoring.h"
#include "monitoring/monitoringConfiguration.h"
#include "version.h"

#include <exception>
#include <future>
#include <iostream>
#include <memory>
#include <random>
#include <regex>
#include <string>

namespace one {
namespace client {
namespace logging {

using namespace one::monitoring; // NOLINT

void startLogging(
    const char *programName, std::shared_ptr<options::Options> options)
{
    try {
        boost::filesystem::create_directories(options->getLogDirPath());
    }
    catch (const boost::filesystem::filesystem_error &e) {
        std::cerr << "Failed to create log directory "
                  << options->getLogDirPath() << ": '" << e.what()
                  << "'. Aborting..." << std::endl;
    }

    FLAGS_log_dir = options->getLogDirPath().c_str();
    FLAGS_stop_logging_if_full_disk = true;
    FLAGS_logtostderr = false;
    FLAGS_v = options->getVerboseLogLevel();
    // Set maximum log size to 50MB plus 50MB for each verbosity level
    constexpr auto kMaximumLogSizeMB = 50;
    FLAGS_max_log_size =
        kMaximumLogSizeMB * (1 + options->getVerboseLogLevel());
    FLAGS_minloglevel = 0;

    google::InitGoogleLogging(programName);

    LOG(INFO) << "Oneclient version: " << ONECLIENT_VERSION;
    LOG(INFO) << "Oneclient commit: " << ONECLIENT_GIT_COMMIT;
    LOG(INFO) << "Helpers commit: " << HELPERS_GIT_COMMIT;
    LOG(INFO) << "Verbose logging level: " << options->getVerboseLogLevel();
    if (options->getProviderHost())
        LOG(INFO) << "Connecting to Oneprovider: "
                  << options->getProviderHost().get();
    LOG(INFO) << "Forced direct IO: " << options->isDirectIOForced();
    LOG(INFO) << "Forced proxy IO: " << options->isProxyIOForced();
    LOG(INFO) << "Verify server certificate: " << !options->isInsecure();
    LOG(INFO) << "File read events disabled: "
              << options->areFileReadEventsDisabled();
    LOG(INFO) << "IO buffered: " << options->isIOBuffered();
    LOG(INFO) << "Oneprovider connection timeout [s]: "
              << options->getProviderTimeout().count();
    LOG(INFO) << "Monitoring enabled: " << options->isMonitoringEnabled();
    if (options->isMonitoringEnabled()) {
        if (options->getMonitoringType())
            LOG(INFO) << "Monitoring type: "
                      << options->getMonitoringType().get();
        LOG(INFO) << "Monitoring level basic: "
                  << options->isMonitoringLevelBasic();
        LOG(INFO) << "Monitoring level full: "
                  << options->isMonitoringLevelFull();
        if (options->getMonitoringGraphiteUrl())
            LOG(INFO) << "Graphite URL: "
                      << options->getMonitoringGraphiteUrl().get();
        if (options->getMonitoringGraphiteNamespacePrefix())
            LOG(INFO) << "Graphite namespace prefix: "
                      << options->getMonitoringGraphiteNamespacePrefix().get();
    }
    LOG(INFO) << "Mountpoint: " << options->getMountpoint();

    google::FlushLogFiles(google::GLOG_INFO);
}

int startPerformanceMonitoring(std::shared_ptr<options::Options> options)
{
    if (options->isMonitoringEnabled()) {
        if (options->getMonitoringType().get() == "graphite") {
            if (!options->getMonitoringGraphiteUrl()) {
                std::cerr << "Graphite URL not specified - use option "
                             "--graphite-url."
                          << std::endl;
                return EXIT_FAILURE;
            }

            auto config = std::make_shared<GraphiteMonitoringConfiguration>();
            try {
                config->fromGraphiteURL(
                    options->getMonitoringGraphiteUrl().get());
            }
            catch (std::invalid_argument &e) {
                std::cerr << "Graphite configuration error: " << e.what()
                          << std::endl;
            }
            if (options->getMonitoringGraphiteNamespacePrefix()) {
                config->namespacePrefix =
                    options->getMonitoringGraphiteNamespacePrefix().get();
            }
            config->reportingPeriod = options->getMonitoringReportingPeriod();
            if (options->isMonitoringLevelFull()) {
                config->reportingLevel = cppmetrics::core::ReportingLevel::Full;
            }
            else {
                config->reportingLevel =
                    cppmetrics::core::ReportingLevel::Basic;
            }

            LOG(INFO) << "Starting Graphite performance monitoring to host: "
                      << config->graphiteHostname;

            // Configure and start monitoring threads
            helpers::configureMonitoring(config, true);

            // Initialize the command line option counter values
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.scheduler_thread_count",
                options->getSchedulerThreadCount());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.communicator_thread_count",
                options->getCommunicatorThreadCount());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.storage_helper_thread_count",
                options->getStorageHelperThreadCount());
            ONE_METRIC_COUNTER_SET("comp.oneclient.mod.options.buffer_"
                                   "scheduler_helper_thread_count",
                options->getBufferSchedulerThreadCount());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.read_buffer_min_size",
                options->getReadBufferMinSize());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.read_buffer_max_size",
                options->getReadBufferMaxSize());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.write_buffer_min_size",
                options->getWriteBufferMinSize());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.write_buffer_max_size",
                options->getWriteBufferMaxSize());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.read_buffer_prefetch_duration",
                options->getReadBufferPrefetchDuration().count());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.write_buffer_flush_delay",
                options->getWriteBufferFlushDelay().count());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.metadata_cache_size",
                options->getMetadataCacheSize());
            ONE_METRIC_COUNTER_SET(
                "comp.oneclient.mod.options.monitoring_reporting_period",
                options->getMonitoringReportingPeriod());
        }
        else {
            std::cerr << "Unsupported performance monitoring reporter: "
                      << options->getMonitoringType().get() << std::endl;
            return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}

} // namespace logging
} // namespace client
} // namespace one
