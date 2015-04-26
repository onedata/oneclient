/**
* @file main.cc
* @author Rafal Slota
* @copyright (C) 2013 ACK CYFRONET AGH
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

#include "shMock.h"
#include "version.h"
#include "context.h"
#include "options.h"
#include "fsLogic.h"
#include "scheduler.h"
#include "scopeExit.h"
#include "oneException.h"
#include "auth/authException.h"
#include "auth/authManager.h"
#include "messages/handshakeResponse.h"

#include <glog/logging.h>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include <array>
#include <exception>
#include <functional>
#include <future>
#include <iostream>
#include <memory>
#include <random>

#include <fcntl.h>
#include <dirent.h>
#include <execinfo.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fuse.h>
#include <fuse/fuse_opt.h>
#include <fuse/fuse_lowlevel.h>

#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif

using namespace one;
using namespace one::client;
using namespace std::placeholders;
using boost::filesystem::path;

namespace {

template <typename... Args1, typename... Args2>
int wrap(int (FsLogic::*operation)(Args2...), Args1 &&... args)
{
    try {
        FsLogic *fsLogic =
            static_cast<FsLogic *>(fuse_get_context()->private_data);
        return (fsLogic->*operation)(std::forward<Args1>(args)...);
    }
    catch (const OneException &e) {
        LOG(ERROR) << "OneException: " << e.what();
        return -EIO;
    }
    catch (...) {
        std::array<void *, 64> trace;

        const auto size = backtrace(trace.data(), trace.size());
        std::unique_ptr<char *[]> strings {
            backtrace_symbols(trace.data(), size)
        };

        LOG(ERROR) << "Unknown exception caught at the top level. Stacktrace: ";
        for (auto i = 0; i < size; ++i)
            LOG(ERROR) << strings[i];

        return -EIO;
    }
}

int wrap_access(const char *path, int mask)
{
    return wrap(&FsLogic::access, path, mask);
}
int wrap_getattr(const char *path, struct stat *statbuf)
{
    return wrap(&FsLogic::getattr, path, statbuf, true);
}
int wrap_readlink(const char *path, char *link, size_t size)
{
    return wrap(&FsLogic::readlink, path, link, size);
}
int wrap_mknod(const char *path, mode_t mode, dev_t dev)
{
    return wrap(&FsLogic::mknod, path, mode, dev);
}
int wrap_mkdir(const char *path, mode_t mode)
{
    return wrap(&FsLogic::mkdir, path, mode);
}
int wrap_unlink(const char *path) { return wrap(&FsLogic::unlink, path); }
int wrap_rmdir(const char *path) { return wrap(&FsLogic::rmdir, path); }
int wrap_symlink(const char *path, const char *link)
{
    return wrap(&FsLogic::symlink, path, link);
}
int wrap_rename(const char *path, const char *newpath)
{
    return wrap(&FsLogic::rename, path, newpath);
}
int wrap_chmod(const char *path, mode_t mode)
{
    return wrap(&FsLogic::chmod, path, mode);
}
int wrap_chown(const char *path, uid_t uid, gid_t gid)
{
    return wrap(&FsLogic::chown, path, uid, gid);
}
int wrap_truncate(const char *path, off_t newSize)
{
    return wrap(&FsLogic::truncate, path, newSize);
}
int wrap_utime(const char *path, struct utimbuf *ubuf)
{
    return wrap(&FsLogic::utime, path, ubuf);
}
int wrap_open(const char *path, struct fuse_file_info *fileInfo)
{
    return wrap(&FsLogic::open, path, fileInfo);
}
int wrap_read(const char *path, char *buf, size_t size, off_t offset,
    struct fuse_file_info *fileInfo)
{
    return wrap(&FsLogic::read, path, buf, size, offset, fileInfo);
}
int wrap_write(const char *path, const char *buf, size_t size, off_t offset,
    struct fuse_file_info *fileInfo)
{
    return wrap(&FsLogic::write, path, buf, size, offset, fileInfo);
}
int wrap_statfs(const char *path, struct statvfs *statInfo)
{
    return wrap(&FsLogic::statfs, path, statInfo);
}
int wrap_flush(const char *path, struct fuse_file_info *fileInfo)
{
    return wrap(&FsLogic::flush, path, fileInfo);
}
int wrap_release(const char *path, struct fuse_file_info *fileInfo)
{
    return wrap(&FsLogic::release, path, fileInfo);
}
int wrap_fsync(const char *path, int datasync, struct fuse_file_info *fi)
{
    return wrap(&FsLogic::fsync, path, datasync, fi);
}
int wrap_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
    off_t offset, struct fuse_file_info *fileInfo)
{
    return wrap(&FsLogic::readdir, path, buf, filler, offset, fileInfo);
}
int wrap_opendir(const char *path, struct fuse_file_info *fileInfo)
{
    return wrap(&FsLogic::opendir, path, fileInfo);
}
int wrap_releasedir(const char *path, struct fuse_file_info *fileInfo)
{
    return wrap(&FsLogic::releasedir, path, fileInfo);
}
int wrap_fsyncdir(
    const char *path, int datasync, struct fuse_file_info *fileInfo)
{
    return wrap(&FsLogic::fsyncdir, path, datasync, fileInfo);
}

void *init(struct fuse_conn_info *conn)
{
    return fuse_get_context()->private_data;
}

std::string generateFuseID()
{
    std::random_device rd;
    std::default_random_engine randomEngine{rd()};
    std::uniform_int_distribution<unsigned long long> fuseIdDistribution;
    return std::to_string(fuseIdDistribution(randomEngine));
}

} // namespace

static struct fuse_operations get_fuse_operations()
{
    struct fuse_operations fuse_oper = {nullptr};

    fuse_oper.init = init;
    fuse_oper.getattr = wrap_getattr;
    fuse_oper.access = wrap_access;
    fuse_oper.readlink = wrap_readlink;
    fuse_oper.readdir = wrap_readdir;
    fuse_oper.mknod = wrap_mknod;
    fuse_oper.mkdir = wrap_mkdir;
    fuse_oper.symlink = wrap_symlink;
    fuse_oper.unlink = wrap_unlink;
    fuse_oper.rmdir = wrap_rmdir;
    fuse_oper.rename = wrap_rename;
    fuse_oper.chmod = wrap_chmod;
    fuse_oper.chown = wrap_chown;
    fuse_oper.truncate = wrap_truncate;
    fuse_oper.open = wrap_open;
    fuse_oper.read = wrap_read;
    fuse_oper.write = wrap_write;
    fuse_oper.statfs = wrap_statfs;
    fuse_oper.release = wrap_release;
    fuse_oper.fsync = wrap_fsync;
    fuse_oper.utime = wrap_utime;
    fuse_oper.flush = wrap_flush;
    fuse_oper.opendir = wrap_opendir;
    fuse_oper.releasedir = wrap_releasedir;
    fuse_oper.fsyncdir = wrap_fsyncdir;

    return fuse_oper;
}

int main(int argc, char *argv[], char *envp[])
{
    google::InitGoogleLogging(argv[0]);

    // Create application context
    auto context = std::make_shared<Context>();

    // Get configuration options
    auto options = std::make_shared<Options>();
    context->setOptions(options);
    try {
        const auto result = options->parseConfigs(argc, argv);
        if (result == Options::Result::HELP) {
            std::cout << "Usage: " << argv[0] << " [options] mountpoint"
                      << std::endl;
            std::cout << options->describeCommandlineOptions() << std::endl;
            return EXIT_SUCCESS;
        }
        if (result == Options::Result::VERSION) {
            std::cout << "oneclient version: " << oneclient_VERSION_MAJOR << "."
                      << oneclient_VERSION_MINOR << "."
                      << oneclient_VERSION_PATCH << std::endl;
            std::cout << "FUSE library version: " << FUSE_MAJOR_VERSION << "."
                      << FUSE_MINOR_VERSION << std::endl;
            return EXIT_SUCCESS;
        }
    }
    catch (OneException &e) {
        std::cerr << "Cannot parse configuration: " << e.what()
                  << ". Check logs for more details. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

    LOG(INFO) << "oneclient version: " << oneclient_VERSION_MAJOR << "."
              << oneclient_VERSION_MINOR << "." << oneclient_VERSION_PATCH
              << std::endl;

    // FUSE main:
    struct fuse *fuse;
    struct fuse_chan *ch;
    struct fuse_operations fuse_oper = get_fuse_operations();
    char *mountpoint;
    int multithreaded;
    int foreground;
    int res;

    struct fuse_args args = options->getFuseArgs();
    res = fuse_parse_cmdline(&args, &mountpoint, &multithreaded, &foreground);
    if (res == -1)
        return EXIT_FAILURE;

    ScopeExit freeMountpoint{[&] { free(mountpoint); }};

    const auto schedulerThreadsNo = options->get_jobscheduler_threads() > 1
        ? options->get_jobscheduler_threads()
        : 1;
    context->setScheduler(std::make_shared<Scheduler>(schedulerThreadsNo));

    std::unique_ptr<auth::AuthManager> authManager;
    try {
        if (options->get_authentication() == "certificate") {
            authManager = std::make_unique<auth::CertificateAuthManager>(
                context, options->get_provider_hostname(),
                options->get_provider_port(),
                !options->get_no_check_certificate(), options->get_debug_gsi());
        }
        else if (options->get_authentication() == "token") {
            authManager = std::make_unique<auth::TokenAuthManager>(context,
                options->get_provider_hostname(), options->get_provider_port(),
                !options->get_no_check_certificate(),
                options->get_global_registry_url(),
                options->get_global_registry_port());
        }
        else {
            throw auth::AuthException{"unknown authentication type: " +
                options->get_authentication()};
        }
    }
    catch (auth::AuthException &e) {
        std::cerr << "Authentication error: " << e.what() << std::endl;
        std::cerr << "Cannot continue. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

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

    // Initialize cluster handshake in order to check if everything is ok before
    // becoming daemon
    const auto fuseId = generateFuseID();

    std::promise<void> handshakePromise;
    auto handshakeFuture = handshakePromise.get_future();
    auto onHandshakeResponse([&](auto response) mutable {
        if (response.sessionId() != fuseId) {
            handshakePromise.set_exception(
                std::make_exception_ptr(OneException{"error"}));
            return false;
        }

        handshakePromise.set_value();
        return true;
    });

    auto testCommunicator = authManager->createCommunicator(
        1, fuseId, std::move(onHandshakeResponse));

    testCommunicator->connect();

    try {
        /// @todo InvalidServerCertificate
        /// @todo More specific errors.
        /// @todo boost::system::system_error throwed on host not found
        handshakeFuture.get();
    }
    catch (OneException &exception) {
        std::cerr << "Handshake error. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

    // cleanup test connections
    testCommunicator.reset();

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

    testCommunicator =
        authManager->createCommunicator(3, fuseId, [](auto) { return true; });

    testCommunicator->connect();

    // Enter FUSE loop
    res = multithreaded ? fuse_loop_mt(fuse) : fuse_loop(fuse);
    return res == -1 ? EXIT_FAILURE : EXIT_SUCCESS;
}
