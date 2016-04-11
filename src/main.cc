/**
 * @file main.cc
 * @author Rafal Slota
 * @copyright (C) 2015 ACK CYFRONET AGH
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
#include "events/eventManager.h"
#include "fsLogic.h"
#include "fsOperations.h"
#include "fuseOperations.h"
#include "logging.h"
#include "messages/configuration.h"
#include "messages/getConfiguration.h"
#include "messages/handshakeResponse.h"
#include "messages/ping.h"
#include "messages/pong.h"
#include "oneException.h"
#include "options.h"
#include "scheduler.h"
#include "scopeExit.h"
#include "shMock.h"
#include "version.h"

#include <fuse/fuse_lowlevel.h>
#include <fuse/fuse_opt.h>

#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>

#include <exception>
#include <functional>
#include <future>
#include <iostream>
#include <memory>
#include <random>
#include <sstream>
#include <string>
#include <vector>

using namespace one;
using namespace one::client;
using namespace std::placeholders;
using namespace std::literals;
using boost::filesystem::path;

std::string generateFuseID()
{
    std::random_device rd;
    std::default_random_engine randomEngine{rd()};
    std::uniform_int_distribution<unsigned long long> fuseIdDistribution;
    return std::to_string(fuseIdDistribution(randomEngine));
}

std::string clientVersion()
{
    std::stringstream stream;
    if (oneclient_VERSION_FALLBACK.empty()) {
        stream << oneclient_VERSION_MAJOR << "." << oneclient_VERSION_MINOR
               << "." << oneclient_VERSION_PATCH;
        if (!oneclient_VERSION_TWEAK.empty()) {
            stream << "-" << oneclient_VERSION_TWEAK;
        }
    }
    else {
        stream << oneclient_VERSION_FALLBACK;
    }

    return stream.str();
}

std::string fuseVersion()
{
    std::stringstream stream;
    stream << FUSE_MAJOR_VERSION << "." << FUSE_MINOR_VERSION;
    return stream.str();
}

void printHelp(const char *name, std::shared_ptr<Options> options)
{
    std::cout << "Usage: " << name << " [options] mountpoint" << std::endl;
    std::cout << options->describeCommandlineOptions() << std::endl;
}

void printVersions()
{
    std::cout << "oneclient version: " << clientVersion() << std::endl;
    std::cout << "FUSE library version: " << fuseVersion() << std::endl;
}

void createScheduler(std::shared_ptr<Context> context)
{
    auto options = context->options();
    const auto schedulerThreadsNo = options->get_jobscheduler_threads() > 1
        ? options->get_jobscheduler_threads()
        : 1;
    context->setScheduler(std::make_shared<Scheduler>(schedulerThreadsNo));
}

std::shared_ptr<auth::AuthManager> createAuthManager(
    std::shared_ptr<Context> context)
{
    auto options = context->options();
    if (options->get_authentication() == "certificate") {
        return std::make_shared<auth::CertificateAuthManager>(
            std::move(context), options->get_provider_hostname(),
            options->get_provider_port(), !options->get_no_check_certificate(),
            options->get_debug_gsi());
    }
    else if (options->get_authentication() == "token") {
        return std::make_shared<auth::TokenAuthManager>(std::move(context),
            options->get_provider_hostname(), options->get_provider_port(),
            !options->get_no_check_certificate());
    }
    else {
        throw auth::AuthException{
            "unknown authentication type: " + options->get_authentication()};
    }
}

std::shared_ptr<communication::Communicator> handshake(
    const std::string &fuseId, std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context)
{
    auto handshakeHandler = [&](auto) { return std::error_code{}; };

    auto testCommunicatorTuple =
        authManager->createCommunicator(1, fuseId, handshakeHandler);
    auto testCommunicator =
        std::get<std::shared_ptr<communication::Communicator>>(
            testCommunicatorTuple);

    testCommunicator->setScheduler(context->scheduler());
    testCommunicator->connect();
    communication::wait(std::get<std::future<void>>(testCommunicatorTuple));

    return testCommunicator;
}

std::shared_ptr<messages::Configuration> getConfiguration(
    std::shared_ptr<communication::Communicator> communicator)
{
    auto future = communicator->communicate<messages::Configuration>(
        messages::GetConfiguration{});
    auto configuration = communication::wait(future);
    return std::make_shared<messages::Configuration>(std::move(configuration));
}

std::shared_ptr<communication::Communicator> createCommunicator(
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context, std::string fuseId)
{
    auto handshakeHandler = [](auto) { return std::error_code{}; };

    auto communicatorTuple =
        authManager->createCommunicator(3, fuseId, handshakeHandler);
    auto communicator = std::get<std::shared_ptr<communication::Communicator>>(
        communicatorTuple);

    communicator->setScheduler(context->scheduler());
    context->setCommunicator(communicator);

    return communicator;
}

boost::filesystem::path getLogDir(std::string name, const Options &options)
{
    if (options.is_default_log_dir()) {
        uid_t uid = geteuid();
        auto userIdent = std::to_string(uid);
        if (auto pw = getpwuid(uid))
            userIdent = pw->pw_name;

        auto log_subdir_name = name + "_" + userIdent + "_logs";
        auto log_path = boost::filesystem::path(options.get_log_dir()) /
            boost::filesystem::path(log_subdir_name).filename();

        boost::filesystem::create_directories(log_path);
        return log_path;
    }

    return {options.get_log_dir()};
}

int main(int argc, char *argv[])
{
    FLAGS_minloglevel = 1;
    FLAGS_logtostderr = true;
    FLAGS_colorlogtostderr = true;
    google::InitGoogleLogging(argv[0]);

    auto context = std::make_shared<Context>();
    const path globalConfigPath = path(oneclient_INSTALL_PATH) /
        oneclient_CONFIG_DIR / GLOBAL_CONFIG_FILE;
    auto options = std::make_shared<Options>(globalConfigPath);
    context->setOptions(options);
    try {
        options->parseConfigs(argc, argv);
    }
    catch (const boost::program_options::error &e) {
        printHelp(argv[0], std::move(options));
        return EXIT_FAILURE;
    }
    catch (const OneException &e) {
        std::cerr << "Cannot parse configuration: " << e.what()
                  << ". Check logs for more details. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

    if (options->get_help()) {
        printHelp(argv[0], std::move(options));
        return EXIT_SUCCESS;
    }
    if (options->get_version()) {
        printVersions();
        return EXIT_SUCCESS;
    }

    google::ShutdownGoogleLogging();

    try {
        FLAGS_log_dir = getLogDir(argv[0], *options).string();
    }
    catch (const boost::filesystem::filesystem_error &e) {
        std::cerr << "Failed to create logdir: " << e.what() << std::endl;
        std::cerr << "Cannot continue. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

    FLAGS_minloglevel = 0;
    FLAGS_logtostderr = false;
    FLAGS_stderrthreshold = options->get_debug() ? 0 : 2;
    google::InitGoogleLogging(argv[0]);

    createScheduler(context);

    std::shared_ptr<auth::AuthManager> authManager;
    try {
        authManager = createAuthManager(context);
    }
    catch (auth::AuthException &e) {
        std::cerr << "Authentication error: " << e.what() << std::endl;
        std::cerr << "Cannot continue. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

    // Initialize cluster handshake in order to check if everything is ok before
    // becoming daemon
    const auto fuseId = generateFuseID();
    std::shared_ptr<messages::Configuration> configuration;
    try {
        /// @todo InvalidServerCertificate
        /// @todo More specific errors.
        /// @todo boost::system::system_error thrown on host not found
        auto communicator = handshake(fuseId, authManager, context);
        std::cout << "Getting configuration..." << std::endl;
        configuration = getConfiguration(std::move(communicator));
    }
    catch (OneException &exception) {
        std::cerr << "Handshake error. Aborting" << std::endl;
        return EXIT_FAILURE;
    }
    catch (const communication::Exception &e) {
        std::cerr << "Error: " << e.what() << ". Aborting." << std::endl;
        return EXIT_FAILURE;
    }
    catch (const std::system_error &e) {
        std::cerr << "Handshake connection error: " << e.what() << ". Aborting"
                  << std::endl;
        return EXIT_FAILURE;
    }

    // FUSE main:
    struct fuse *fuse;
    struct fuse_chan *ch;
    struct fuse_operations fuse_oper = fuseOperations();
    char *mountpoint;
    int multithreaded;
    int foreground;
    int res;

    struct fuse_args args = options->getFuseArgs();
    res = fuse_parse_cmdline(&args, &mountpoint, &multithreaded, &foreground);
    if (res == -1)
        return EXIT_FAILURE;

    ScopeExit freeMountpoint{[&] { free(mountpoint); }};

    ch = fuse_mount(mountpoint, &args);
    if (!ch)
        return EXIT_FAILURE;

    ScopeExit unmountFuse{[&] { fuse_unmount(mountpoint, ch); }};

    res = fcntl(fuse_chan_fd(ch), F_SETFD, FD_CLOEXEC);
    if (res == -1)
        perror("WARNING: failed to set FD_CLOEXEC on fuse device");

    FsLogicWrapper fsLogicWrapper;
    fuse = helpers::fuseNew(
        ch, &args, &fuse_oper, sizeof(fuse_oper), &fsLogicWrapper);
    if (fuse == nullptr)
        return EXIT_FAILURE;

    ScopeExit destroyFuse{[&] { fuse_destroy(fuse); }, unmountFuse};

    fuse_set_signal_handlers(fuse_get_session(fuse));
    ScopeExit removeHandlers{
        [&] { fuse_remove_signal_handlers(fuse_get_session(fuse)); }};

    std::cout << "oneclient has been successfully mounted in " << mountpoint
              << std::endl;

    if (!foreground) {
        context->scheduler()->prepareForDaemonize();

        fuse_remove_signal_handlers(fuse_get_session(fuse));
        res = fuse_daemonize(foreground);

        if (res != -1)
            res = fuse_set_signal_handlers(fuse_get_session(fuse));

        if (res == -1)
            return EXIT_FAILURE;

        context->scheduler()->restartAfterDaemonize();
    }
    else {
        // in foreground, log at least warning messages
        FLAGS_stderrthreshold = options->get_debug() ? 0 : 1;
    }

    auto communicator =
        createCommunicator(authManager, context, std::move(fuseId));
    communicator->connect();

    fsLogicWrapper.logic =
        std::make_unique<FsLogic>(std::move(context), std::move(configuration));

    // Enter FUSE loop
    res = multithreaded ? fuse_loop_mt(fuse) : fuse_loop(fuse);

    communicator->stop();
    return res == -1 ? EXIT_FAILURE : EXIT_SUCCESS;
}
