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
#if FUSE_USE_VERSION > 30
#include <fuse3/fuse_lowlevel.h>
#include <fuse3/fuse_opt.h>
#else
#include <fuse/fuse_lowlevel.h>
#include <fuse/fuse_opt.h>
#endif
#include <macaroons.hpp>

#include <sys/mount.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <csignal>
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

namespace {
std::shared_ptr<options::Options> __options{};
} // namespace

std::shared_ptr<options::Options> getOptions(int argc, char *argv[])
{
    auto options = std::make_shared<options::Options>();
    try {
        options->parse(argc, argv);
        return options;
    }
    catch (const boost::program_options::error &e) {
        fmt::print(stderr, "{}\nSee '{} --help'\n",
            std::regex_replace(e.what(), std::regex("--"), ""), argv[0]);
        exit(EXIT_FAILURE);
    }
}

void sigtermHandler(int signum)
{
    if (!__options)
        exit(signum);

    fmt::print(stderr,
        "Oneclient received ({}) signal - releasing mountpoint: {}\n", signum,
        __options->getMountpoint().c_str());

#if FUSE_USE_VERSION > 30
    const auto *exec = "/bin/fusermount3";
#else
    auto exec = "/bin/fusermount";
#endif
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    execl(exec, exec, "-uz", __options->getMountpoint().c_str(), NULL);

    exit(signum);
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
        execl(exec, exec, "unmount", options->getMountpoint().c_str(), nullptr);
#else
#if FUSE_USE_VERSION > 30
        const auto *exec = "/bin/fusermount3";
#else
        auto exec = "/bin/fusermount";
#endif
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
        execl(exec, exec, "-uz", options->getMountpoint().c_str(), nullptr);
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
    __options = options;
    context->setOptions(options);

    if (options->getHelp()) {
        std::cout << options->formatHelp(argv[0]);
        return EXIT_SUCCESS;
    }
    if (options->getVersion()) {
        fmt::print("Oneclient: {}\nFUSE library: {}.{}\n", ONECLIENT_VERSION,
            FUSE_MAJOR_VERSION, FUSE_MINOR_VERSION);
        return EXIT_SUCCESS;
    }
    if (options->getUnmount()) {
        unmountFuse(options);
    }
    if (!options->getProviderHost()) {
        fmt::print(stderr,
            "The option 'host' is required but missing\nSee '{} --help'.\n",
            argv[0]);
        return EXIT_FAILURE;
    }
    if (options->hasDeprecated()) {
        std::cout << options->formatDeprecated();
    }

    startLogging(argv[0], options);

    context->setScheduler(
        std::make_shared<Scheduler>(options->getSchedulerThreadCount()));

    int res{};

    try {
        auto fuse_oper = fuseOperations();
        auto args = options->getFuseArgs(argv[0]);
        char *mountpoint{nullptr};
        int multithreaded{0};
        int foreground{0};
        struct fuse_session *fuse = nullptr;

#if FUSE_USE_VERSION > 30
        struct fuse_cmdline_opts opts;
        res = fuse_parse_cmdline(&args, &opts);
        if (res == -1)
            return EXIT_FAILURE;

        multithreaded = !opts.singlethread; // NOLINT
        foreground = opts.foreground;
        mountpoint = opts.mountpoint;

        if (foreground == 0) {
            FLAGS_stderrthreshold = 3;
        }
        else {
            FLAGS_stderrthreshold = options->getDebug() ? 0 : 1;
        }

        ScopeExit freeMountpoint{[=] {
            free(mountpoint); // NOLINT
        }};

        // Create test communicator with single connection to test the
        // authentication and get protocol configuration
        auto authManager = getAuthManager(context);
        auto sessionId = generateSessionId();
        auto configuration = getConfiguration(sessionId, authManager, context);

        if (!configuration)
            return EXIT_FAILURE;

        std::unique_ptr<fslogic::Composite> fsLogic;
        fuse = fuse_session_new(&args, &fuse_oper, sizeof(fuse_oper), &fsLogic);
        if (fuse == nullptr)
            return EXIT_FAILURE;

        res = fuse_set_signal_handlers(fuse);
        if (res == -1)
            return EXIT_FAILURE;

        ScopeExit removeHandlers{[=] { fuse_remove_signal_handlers(fuse); }};

        res = fuse_session_mount(fuse, mountpoint);
        if (res != 0)
            return EXIT_FAILURE;

        ScopeExit unmountFuse{[=] { fuse_session_unmount(fuse); }};
        ScopeExit destroyFuse{[=] { fuse_session_destroy(fuse); }, unmountFuse};

        std::signal(SIGINT, sigtermHandler);
        std::signal(SIGTERM, sigtermHandler);

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
#else
        res =
            fuse_parse_cmdline(&args, &mountpoint, &multithreaded, &foreground);
        if (res == -1)
            return EXIT_FAILURE;

        if (foreground == 0) {
            FLAGS_stderrthreshold = 3;
        }
        else {
            FLAGS_stderrthreshold = options->getDebug() ? 0 : 1;
        }

        // Create test communicator with single connection to test the
        // authentication and get protocol configuration
        auto authManager = getAuthManager(context);
        auto sessionId = generateSessionId();
        auto configuration = getConfiguration(sessionId, authManager, context);

        if (!configuration)
            return EXIT_FAILURE;

        ScopeExit freeMountpoint{[=] {
            free(mountpoint); // NOLINT
        }};

        auto ch = fuse_mount(mountpoint, &args);
        if (ch == nullptr)
            return EXIT_FAILURE;

        ScopeExit unmountFuse{[=] { fuse_unmount(mountpoint, ch); }};

        std::signal(SIGINT, sigtermHandler);
        std::signal(SIGTERM, sigtermHandler);

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

            if (res == -1) {
                return EXIT_FAILURE;
            }

            folly::SingletonVault::singleton()->reenableInstances();
            context->scheduler()->restartAfterDaemonize();
        }
#endif

        if (startPerformanceMonitoring(options) != EXIT_SUCCESS)
            return EXIT_FAILURE;

        auto communicator = getCommunicator(sessionId, authManager, context);
        context->setCommunicator(communicator);
        communicator->connect();
        communicator->schedulePeriodicMessageRequest();
        authManager->scheduleRefresh(auth::RESTRICTED_MACAROON_REFRESH);

        auto helpersCache = std::make_unique<cache::HelpersCache>(
            *communicator, context->scheduler(), *options);

        const auto &rootUuid = configuration->rootUuid();
        fsLogic = std::make_unique<fslogic::Composite>(rootUuid,
            std::move(context), std::move(configuration),
            std::move(helpersCache), options->getMetadataCacheSize(),
            options->areFileReadEventsDisabled(),
            options->isFullblockReadEnabled(), options->getProviderTimeout(),
            options->getDirectoryCacheDropAfter());

#if FUSE_USE_VERSION > 31
        struct fuse_loop_config config;
        config.clone_fd = opts.clone_fd;
        config.max_idle_threads = opts.max_idle_threads;
        res = (multithreaded != 0) ? fuse_session_loop_mt(fuse, &config)
                                   : fuse_session_loop(fuse);
#elif FUSE_VERSION == 31
        res = (multithreaded != 0) ? fuse_session_loop_mt(fuse, opts.clone_fd)
                                   : fuse_session_loop(fuse);
#else
        res = (multithreaded != 0) ? fuse_session_loop_mt(fuse)
                                   : fuse_session_loop(fuse);
#endif
    }
    catch (const macaroons::exception::Invalid &e) {
        fmt::print(stderr,
            "ERROR: Cannot parse token - please make sure that the access "
            "token has been copied correctly.\n");
        return EXIT_FAILURE;
    }
    catch (const macaroons::exception::NotAuthorized &e) {
        fmt::print(stderr,
            "ERROR: Invalid token - please make sure that the access token is "
            "valid for Oneclient access to Oneprovider: {}\n",
            *__options->getProviderHost());
        return EXIT_FAILURE;
    }
    catch (const std::system_error &e) {
        using one::errors::handshake::ErrorCode;

        if (e.code() == ErrorCode::macaroon_expired)
            fmt::print(stderr,
                "ERROR: Expired token - the provided token is "
                "expired, please create a new one.\n");
        else if (e.code() == ErrorCode::invalid_macaroon ||
            e.code() == ErrorCode::invalid_provider ||
            e.code() == ErrorCode::macaroon_not_found)
            fmt::print(stderr,
                "ERROR: Invalid token - the provided token is not valid for "
                "Oneclient access to Oneprovider: {}\n",
                *__options->getProviderHost());
        else if (e.code() == ErrorCode::incompatible_version)
            fmt::print(stderr,
                "ERROR: This Oneclient version ({}) is not compatible with "
                "this Oneprovider, see: "
                "https://{}/api/v3/oneprovider/configuration\nPlease also "
                "consult the current Onedata compatibility matrix: "
                "https://onedata.org/#/home/versions\n",
                ONECLIENT_VERSION, *__options->getProviderHost());
        else {
            fmt::print(stderr, "ERROR: Cannot connect to Oneprovider {} - {}\n",
                *__options->getProviderHost(), e.what());
        }

        return EXIT_FAILURE;
    }
    catch (const folly::AsyncSocketException &e) {
        std::string message;
        using fas = folly::AsyncSocketException;
        if (e.getType() == fas::AsyncSocketExceptionType::SSL_ERROR) {
            message = "SSL socket creation failed, if the Oneprovider has "
                      "self-hosted certificate add option '-i'";
        }
        else if (e.getType() == fas::AsyncSocketExceptionType::TIMED_OUT) {
            message = "Connection timed out, please make sure the Oneprovider "
                      "hostname is correct and reachable from this host.";
        }
        else {
            message = e.what();
        }

        fmt::print(stderr, "ERROR: Cannot connect to Oneprovider {} - {}\n",
            *__options->getProviderHost(), message);

        return EXIT_FAILURE;
    }
    catch (const std::exception &e) {
        fmt::print(stderr, "ERROR: Cannot connect to Oneprovider {} - {}\n",
            *__options->getProviderHost(), e.what());
    }

    return res == -1 ? EXIT_FAILURE : EXIT_SUCCESS;
}
