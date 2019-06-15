/**
 * @file main.cc
 * @author Rafal Slota
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif

#include "asio.hpp"
#include "auth/authException.h"
#include "auth/authManager.h"
#include "communication/exception.h"
#include "configuration.h"
#include "context.h"
#include "errors/handshakeErrors.h"
#include "fsOperations.h"
#include "fslogic/composite.h"
#include "fuseOperations.h"
#include "helpers/init.h"
#include "helpers/logging.h"
#include "messages/configuration.h"
#include "messages/getConfiguration.h"
#include "messages/handshakeResponse.h"
#include "monitoring/monitoring.h"
#include "monitoring/monitoringConfiguration.h"
#include "options/options.h"
#include "scheduler.h"
#include "scopeExit.h"
#include "version.h"

#include <folly/Singleton.h>
#include <fuse/fuse_lowlevel.h>
#include <fuse/fuse_opt.h>

#include <sys/mount.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <exception>
#include <future>
#include <iostream>
#include <memory>
#include <random>
#include <regex>
#include <string>

using namespace one;             // NOLINT
using namespace one::client;     // NOLINT
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
    LOG(INFO) << "Compatible Oneprovider versions: ";
    for (const auto &version : ONECLIENT_COMPATIBLE_ONEPROVIDER_VERSIONS)
        LOG(INFO) << version << " ";
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

std::shared_ptr<options::Options> getOptions(int argc, char *argv[])
{
    auto options = std::make_shared<options::Options>();
    try {
        options->parse(argc, argv);
        return options;
    }
    catch (const boost::program_options::error &e) {
        std::cerr << std::regex_replace(e.what(), std::regex("--"), "") << "\n"
                  << "See '" << argv[0] << " --help'." << std::endl;
        exit(EXIT_FAILURE);
    }
}

void unmountFuse(std::shared_ptr<options::Options> options)
{
    int status = 0;
    int pid = fork();

    if (pid != 0) {
        waitpid(pid, &status, 0);
    }
    else {
#if defined(__APPLE__)
        auto exec = "/usr/sbin/diskutil";
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
        execl(exec, exec, "unmount", options->getMountpoint().c_str(), NULL);
#else
        auto exec = "/bin/fusermount";
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
        execl(exec, exec, "-u", options->getMountpoint().c_str(), NULL);
#endif
    }
    if (status == 0) {
        std::cout << "Oneclient has been successfully unmounted." << std::endl;
    }
    exit(status);
}

int main(int argc, char *argv[])
{
    helpers::init();

    auto context = std::make_shared<Context>();
    auto options = getOptions(argc, argv);
    context->setOptions(options);

    if (options->getHelp()) {
        std::cout << options->formatHelp(argv[0]);
        return EXIT_SUCCESS;
    }
    if (options->getVersion()) {
        std::cout << "Oneclient: " << ONECLIENT_VERSION << "\n"
                  << "FUSE library: " << FUSE_MAJOR_VERSION << "."
                  << FUSE_MINOR_VERSION << std::endl;
        return EXIT_SUCCESS;
    }
    if (options->getUnmount()) {
        unmountFuse(options);
    }
    if (!options->getProviderHost()) {
        std::cerr << "the option 'host' is required but missing\n"
                  << "See '" << argv[0] << " --help'." << std::endl;
        return EXIT_FAILURE;
    }
    if (options->hasDeprecated()) {
        std::cout << options->formatDeprecated();
    }

    startLogging(argv[0], options);

    context->setScheduler(
        std::make_shared<Scheduler>(options->getSchedulerThreadCount()));

    auto authManager = getAuthManager(context);
    auto sessionId = generateSessionId();
    auto configuration = getConfiguration(sessionId, authManager, context);

    auto fuse_oper = fuseOperations();
    auto args = options->getFuseArgs(argv[0]);
    char *mountpoint;
    int multithreaded;
    int foreground;
    int res;

    res = fuse_parse_cmdline(&args, &mountpoint, &multithreaded, &foreground);
    if (res == -1)
        return EXIT_FAILURE;

    ScopeExit freeMountpoint{[=] {
        free(mountpoint); // NOLINT
    }};

    auto ch = fuse_mount(mountpoint, &args);
    if (ch == nullptr)
        return EXIT_FAILURE;

    ScopeExit unmountFuse{[=] { fuse_unmount(mountpoint, ch); }};

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    res = fcntl(fuse_chan_fd(ch), F_SETFD, FD_CLOEXEC);
    if (res == -1)
        perror("WARNING: failed to set FD_CLOEXEC on fuse device");

    std::unique_ptr<fslogic::Composite> fsLogic;
    auto fuse =
        fuse_lowlevel_new(&args, &fuse_oper, sizeof(fuse_oper), &fsLogic);
    if (fuse == nullptr)
        return EXIT_FAILURE;

    ScopeExit destroyFuse{[=] { fuse_session_destroy(fuse); }, unmountFuse};

    fuse_set_signal_handlers(fuse);
    ScopeExit removeHandlers{[&] { fuse_remove_signal_handlers(fuse); }};

    fuse_session_add_chan(fuse, ch);
    ScopeExit removeChannel{[&] { fuse_session_remove_chan(ch); }};

    std::cout << "Oneclient has been successfully mounted in '"
              << options->getMountpoint().c_str() << "'." << std::endl;

    if (foreground == 0) {
        context->scheduler()->prepareForDaemonize();
        folly::SingletonVault::singleton()->destroyInstances();

        fuse_remove_signal_handlers(fuse);
        res = fuse_daemonize(foreground);

        if (res != -1)
            res = fuse_set_signal_handlers(fuse);

        if (res == -1)
            return EXIT_FAILURE;

        folly::SingletonVault::singleton()->reenableInstances();
        context->scheduler()->restartAfterDaemonize();
    }
    else {
        FLAGS_stderrthreshold = options->getDebug() ? 0 : 1;
    }

    if (startPerformanceMonitoring(options) != EXIT_SUCCESS)
        return EXIT_FAILURE;

    auto communicator = getCommunicator(sessionId, authManager, context);
    context->setCommunicator(communicator);
    communicator->connect();

    auto helpersCache = std::make_unique<cache::HelpersCache>(
        *communicator, *context->scheduler(), *options);

    const auto &rootUuid = configuration->rootUuid();
    fsLogic = std::make_unique<fslogic::Composite>(rootUuid, std::move(context),
        std::move(configuration), std::move(helpersCache),
        options->getMetadataCacheSize(), options->areFileReadEventsDisabled(),
        options->isFullblockReadEnabled(), options->getProviderTimeout());

    res = (multithreaded != 0) ? fuse_session_loop_mt(fuse)
                               : fuse_session_loop(fuse);

    return res == -1 ? EXIT_FAILURE : EXIT_SUCCESS;
}
