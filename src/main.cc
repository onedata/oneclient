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
#include "context.h"
#include "fsOperations.h"
#include "fslogic/composite.h"
#include "fuseOperations.h"
#include "helpers/init.h"
#include "logging.h"
#include "messages/configuration.h"
#include "messages/getConfiguration.h"
#include "messages/handshakeResponse.h"
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

using namespace one;
using namespace one::client;

void startLogging(
    const char *programName, std::shared_ptr<options::Options> options)
{
    try {
        boost::filesystem::create_directories(options->getLogDirPath());
    }
    catch (const boost::filesystem::filesystem_error &e) {
        std::cerr << "Failed to create log directory: '" << e.what()
                  << "'. Aborting..." << std::endl;
    }

    FLAGS_minloglevel = 0;
    FLAGS_logtostderr = false;
    FLAGS_stderrthreshold = options->getDebug() ? 0 : 2;
    FLAGS_log_dir = options->getLogDirPath().c_str();
    google::InitGoogleLogging(programName);
}

std::string generateSessionId()
{
    std::random_device rd;
    std::default_random_engine randomEngine{rd()};
    std::uniform_int_distribution<unsigned long long> sessionIdDistribution;
    return std::to_string(sessionIdDistribution(randomEngine));
}

std::shared_ptr<communication::Communicator> handshake(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context)
{
    auto handshakeHandler = [&](messages::HandshakeResponse msg) {
        if (msg.isTokenError()) {
            authManager->cleanup();
        }
        return msg.status();
    };

    auto testCommunicatorTuple =
        authManager->createCommunicator(1, sessionId, handshakeHandler);
    auto testCommunicator =
        std::get<std::shared_ptr<communication::Communicator>>(
            testCommunicatorTuple);

    testCommunicator->setScheduler(context->scheduler());
    testCommunicator->connect();
    communication::wait(
        std::get<folly::Future<folly::Unit>>(testCommunicatorTuple));

    return testCommunicator;
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

std::shared_ptr<auth::AuthManager> getAuthManager(
    std::shared_ptr<Context> context)
{
    try {
        auto options = context->options();
        return std::make_shared<auth::TokenAuthManager>(context,
            options->getProviderHost().get(), options->getProviderPort(),
            !options->isInsecure());
    }
    catch (auth::AuthException &e) {
        std::cerr << "Authentication error: '" << e.what() << "'. Aborting..."
                  << std::endl;
        exit(EXIT_FAILURE);
    }
}

std::shared_ptr<messages::Configuration> getConfiguration(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context)
{
    auto options = context->options();
    std::cout << "Connecting to provider '" << options->getProviderHost().get()
              << ":" << options->getProviderPort() << "' using session ID: '"
              << sessionId << "'..." << std::endl;

    try {
        auto communicator =
            handshake(sessionId, std::move(authManager), std::move(context));

        std::cout << "Getting configuration..." << std::endl;

        auto future = communicator->communicate<messages::Configuration>(
            messages::GetConfiguration{});
        auto configuration = communication::wait(future);
        return std::make_shared<messages::Configuration>(
            std::move(configuration));
    }
    catch (const std::exception &e) {
        std::cerr << "Connection error: '" << e.what() << "'. Aborting..."
                  << std::endl;
        exit(EXIT_FAILURE);
    }
}

std::shared_ptr<communication::Communicator> getCommunicator(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context)
{
    auto handshakeHandler = [](auto) { return std::error_code{}; };

    auto communicatorTuple = authManager->createCommunicator(
        context->options()->getCommunicatorThreadCount(), sessionId,
        handshakeHandler);
    auto communicator = std::get<std::shared_ptr<communication::Communicator>>(
        communicatorTuple);

    communicator->setScheduler(context->scheduler());

    return communicator;
}

void unmountFuse(std::shared_ptr<options::Options> options)
{
    int status = 0, pid = fork();
    if (pid) {
        waitpid(pid, &status, 0);
    }
    else {
#if defined(__APPLE__)
        auto exec = "/usr/sbin/diskutil";
        execl(exec, exec, "unmount", options->getMountpoint().c_str(), NULL);
#else
        auto exec = "/bin/fusermount";
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

    ScopeExit freeMountpoint{[=] { free(mountpoint); }};

    auto ch = fuse_mount(mountpoint, &args);
    if (!ch)
        return EXIT_FAILURE;

    ScopeExit unmountFuse{[=] { fuse_unmount(mountpoint, ch); }};

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

    if (!foreground) {
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

    auto communicator = getCommunicator(sessionId, authManager, context);
    context->setCommunicator(communicator);
    communicator->connect();

    auto helpersCache = std::make_unique<cache::HelpersCache>(
        *communicator, *context->scheduler(), *options);

    const auto &rootUuid = configuration->rootUuid();
    fsLogic = std::make_unique<fslogic::Composite>(rootUuid, std::move(context),
        std::move(configuration), std::move(helpersCache),
        options->getMetadataCacheSize());

    res = multithreaded ? fuse_session_loop_mt(fuse) : fuse_session_loop(fuse);

    communicator->stop();
    return res == -1 ? EXIT_FAILURE : EXIT_SUCCESS;
}
