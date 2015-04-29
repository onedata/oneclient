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

#include "fsInit.h"
#include "fsUtils.h"
#include "shMock.h"
#include "scopeExit.h"
#include "events/eventManager.h"

#include <glog/logging.h>

#include <fuse/fuse_opt.h>
#include <fuse/fuse_lowlevel.h>

using namespace one::client;

int main(int argc, char *argv[])
{
    initializeLogging(argv[0]);

    auto context = std::make_shared<Context>();
    auto options = std::make_shared<Options>();
    context->setOptions(options);
    try {
        const auto result = options->parseConfigs(argc, argv);
        if (result == Options::Result::HELP) {
            printHelp(argv[0], std::move(options));
            return EXIT_SUCCESS;
        }
        if (result == Options::Result::VERSION) {
            printVersions();
            return EXIT_SUCCESS;
        }
    }
    catch (OneException &e) {
        std::cerr << "Cannot parse configuration: " << e.what()
                  << ". Check logs for more details. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

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
    std::string fuseId;
    try {
        /// @todo InvalidServerCertificate
        /// @todo More specific errors.
        /// @todo boost::system::system_error thrown on host not found
        fuseId = handshake(authManager);
    }
    catch (OneException &exception) {
        std::cerr << "Handshake error. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

    auto communicator =
        createCommunicator(authManager, context, std::move(fuseId));

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

    FsLogic *fsLogic = new FsLogic(mountpoint, context);
    ScopeExit destroyFsLogic{[&] { delete fsLogic; }};

    fuse = fuse_new(
        ch, &args, &fuse_oper, sizeof(struct fuse_operations), fsLogic);
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

    communicator->connect();

    // Enter FUSE loop
    res = multithreaded ? fuse_loop_mt(fuse) : fuse_loop(fuse);
    return res == -1 ? EXIT_FAILURE : EXIT_SUCCESS;
}
