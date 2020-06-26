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
#include "logging.h"
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

using namespace one;                  // NOLINT
using namespace one::client;          // NOLINT
using namespace one::client::logging; // NOLINT
using namespace one::monitoring;      // NOLINT

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

    if (!configuration)
        return EXIT_FAILURE;

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
        options->isFullblockReadEnabled(), options->getProviderTimeout(),
        options->getDirectoryCacheDropAfter());

    res = (multithreaded != 0) ? fuse_session_loop_mt(fuse)
                               : fuse_session_loop(fuse);

    return res == -1 ? EXIT_FAILURE : EXIT_SUCCESS;
}
