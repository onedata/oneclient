/**
 * @file veilFuse.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif

#include "certUnconfirmedException.h"
#include "communication/communicator.h"
#include "communication/communicationHandler.h"
#include "communication/websocketConnectionPool.h"
#include "communication/websocketConnection.h"
#include "config.h"
#include "context.h"
#include "events/eventCommunicator.h"
#include "fslogicProxy.h"
#include "grAdapter.h"
#include "gsiHandler.h"
#include "helpers/storageHelperFactory.h"
#include "ISchedulable.h"
#include "jobScheduler.h"
#include "localStorageManager.h"
#include "logging.h"
#include "make_unique.h"
#include "metaCache.h"
#include "options.h"
#include "pushListener.h"
#include "scopeExit.h"
#include "storageMapper.h"
#include "veilConfig.h"
#include "veilException.h"
#include "veilfs.h"

#include <dirent.h>
#include <fcntl.h>
#include <fuse.h>
#include <fuse/fuse_lowlevel.h>
#include <fuse/fuse_opt.h>
#include <pwd.h>
#include <sys/stat.h>
#include <unistd.h>
#include <unistd.h>
#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include <functional>
#include <iostream>
#include <memory>

using namespace std;
using namespace std::placeholders;
using namespace veil;
using namespace veil::client;
using boost::filesystem::path;

/// Main  application object (filesystem state)
static std::weak_ptr<VeilFS> VeilAppObject;

extern "C"
{
    static int wrap_access(const char *path, int mask)
    {
        return VeilAppObject.lock()->access(path, mask);
    }
    static int wrap_getattr(const char *path, struct stat *statbuf)
    {
        return VeilAppObject.lock()->getattr(path, statbuf);
    }
    static int wrap_readlink(const char *path, char *link, size_t size)
    {
        return VeilAppObject.lock()->readlink(path, link, size);
    }
    static int wrap_mknod(const char *path, mode_t mode, dev_t dev)
    {
        return VeilAppObject.lock()->mknod(path, mode, dev);
    }
    static int wrap_mkdir(const char *path, mode_t mode)
    {
        return VeilAppObject.lock()->mkdir(path, mode);
    }
    static int wrap_unlink(const char *path)
    {
        return VeilAppObject.lock()->unlink(path);
    }
    static int wrap_rmdir(const char *path)
    {
        return VeilAppObject.lock()->rmdir(path);
    }
    static int wrap_symlink(const char *path, const char *link)
    {
        return VeilAppObject.lock()->symlink(path, link);
    }
    static int wrap_rename(const char *path, const char *newpath)
    {
        return VeilAppObject.lock()->rename(path, newpath);
    }
    static int wrap_link(const char *path, const char *newpath)
    {
        return VeilAppObject.lock()->link(path, newpath);
    }
    static int wrap_chmod(const char *path, mode_t mode)
    {
        return VeilAppObject.lock()->chmod(path, mode);
    }
    static int wrap_chown(const char *path, uid_t uid, gid_t gid)
    {
        return VeilAppObject.lock()->chown(path, uid, gid);
    }
    static int wrap_truncate(const char *path, off_t newSize)
    {
        return VeilAppObject.lock()->truncate(path, newSize);
    }
    static int wrap_utime(const char *path, struct utimbuf *ubuf)
    {
        return VeilAppObject.lock()->utime(path, ubuf);
    }
    static int wrap_open(const char *path, struct fuse_file_info *fileInfo)
    {
        return VeilAppObject.lock()->open(path, fileInfo);
    }
    static int wrap_read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo)
    {
        return VeilAppObject.lock()->read(path, buf, size, offset, fileInfo);
    }
    static int wrap_write(const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo)
    {
        return VeilAppObject.lock()->write(path, buf, size, offset, fileInfo);
    }
    static int wrap_statfs(const char *path, struct statvfs *statInfo)
    {
        return VeilAppObject.lock()->statfs(path, statInfo);
    }
    static int wrap_flush(const char *path, struct fuse_file_info *fileInfo)
    {
        return VeilAppObject.lock()->flush(path, fileInfo);
    }
    static int wrap_release(const char *path, struct fuse_file_info *fileInfo)
    {
        return VeilAppObject.lock()->release(path, fileInfo);
    }
    static int wrap_fsync(const char *path, int datasync, struct fuse_file_info *fi)
    {
        return VeilAppObject.lock()->fsync(path, datasync, fi);
    }
    #ifdef HAVE_SETXATTR
    static int wrap_setxattr(const char *path, const char *name, const char *value, size_t size, int flags)
    {
        return VeilAppObject.lock()->setxattr(path, name, value, size, flags);
    }
    static int wrap_getxattr(const char *path, const char *name, char *value, size_t size)
    {
        return VeilAppObject.lock()->getxattr(path, name, value, size);
    }
    static int wrap_listxattr(const char *path, char *list, size_t size)
    {
        return VeilAppObject.lock()->listxattr(path, list, size);
    }
    static int wrap_removexattr(const char *path, const char *name)
    {
        return VeilAppObject.lock()->removexattr(path, name);
    }
    #endif // HAVE_SETXATTR
    static int wrap_readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo)
    {
        return VeilAppObject.lock()->readdir(path, buf, filler, offset, fileInfo);
    }
    static int wrap_opendir(const char *path, struct fuse_file_info *fileInfo)
    {
        return VeilAppObject.lock()->opendir(path, fileInfo);
    }
    static int wrap_releasedir(const char *path, struct fuse_file_info *fileInfo)
    {
        return VeilAppObject.lock()->releasedir(path, fileInfo);
    }
    static int wrap_fsyncdir(const char *path, int datasync, struct fuse_file_info *fileInfo)
    {
        return VeilAppObject.lock()->fsyncdir(path, datasync, fileInfo);
    }
//    static int wrap_init(struct fuse_conn_info *conn)
//    {
//        return VeilAppObject.lock()->init(conn);
//    }

} // extern "C"

static struct fuse_operations fuse_init() {
    struct fuse_operations vfs_oper = {0};

    vfs_oper.getattr    = wrap_getattr;
    vfs_oper.access     = wrap_access;
    vfs_oper.readlink   = wrap_readlink;
    vfs_oper.readdir    = wrap_readdir;
    vfs_oper.mknod      = wrap_mknod;
    vfs_oper.mkdir      = wrap_mkdir;
    vfs_oper.symlink    = wrap_symlink;
    vfs_oper.unlink     = wrap_unlink;
    vfs_oper.rmdir      = wrap_rmdir;
    vfs_oper.rename     = wrap_rename;
    vfs_oper.link       = wrap_link;
    vfs_oper.chmod      = wrap_chmod;
    vfs_oper.chown      = wrap_chown;
    vfs_oper.truncate   = wrap_truncate;
#ifdef HAVE_UTIMENSAT
    vfs_oper.utimens    = wrap_utimens;
#endif
    vfs_oper.open       = wrap_open;
    vfs_oper.read       = wrap_read;
    vfs_oper.write      = wrap_write;
    vfs_oper.statfs     = wrap_statfs;
    vfs_oper.release    = wrap_release;
    vfs_oper.fsync      = wrap_fsync;
    vfs_oper.utime      = wrap_utime;
    vfs_oper.flush      = wrap_flush;
    vfs_oper.opendir    = wrap_opendir;
    vfs_oper.releasedir = wrap_releasedir;
    vfs_oper.fsyncdir   = wrap_fsyncdir;
#ifdef HAVE_POSIX_FALLOCATE
    vfs_oper.fallocate  = wrap_fallocate;
#endif
#ifdef HAVE_SETXATTR
    vfs_oper.setxattr   = wrap_setxattr;
    vfs_oper.getxattr   = wrap_getxattr;
    vfs_oper.listxattr  = wrap_listxattr;
    vfs_oper.removexattr= wrap_removexattr;
#endif

    return vfs_oper;
}

static std::string getVersionString()
{
    std::stringstream ss;
    ss << VeilClient_VERSION_MAJOR << "."
        << VeilClient_VERSION_MINOR << "."
        << VeilClient_VERSION_PATCH;
    return ss.str();
}
#include <regex>
int main(int argc, char* argv[], char* envp[])
{
    // Turn off logging for a while
    google::InitGoogleLogging(argv[0]);
    FLAGS_alsologtostderr = false;
    FLAGS_logtostderr = false;
    FLAGS_stderrthreshold = 3;

    // Set up a remote logger
    const auto logWriter = std::make_shared<logging::RemoteLogWriter>();
    const auto logSink = std::make_shared<logging::RemoteLogSink>(logWriter);
    const auto debugLogSink = std::make_shared<logging::RemoteLogSink>(logWriter, protocol::logging::LDEBUG);
    logging::setLogSinks(logSink, debugLogSink);

    // Create application context
    auto context = std::make_shared<Context>();

    // Initialize FUSE
    umask(0);
    auto vfs_oper = fuse_init();

    // Get configuration options
    auto options = std::make_shared<Options>();
    context->setOptions(options);
    try
    {
        const auto result = options->parseConfigs(argc, argv);
        if(result == Options::Result::HELP)
        {
            std::cout << "Usage: " << argv[0] << " [options] mountpoint" << std::endl;
            std::cout << options->describeCommandlineOptions() << std::endl;
            return EXIT_SUCCESS;
        }
        if(result == Options::Result::VERSION)
        {
            std::cout << "VeilFuse version: " << VeilClient_VERSION_MAJOR << "." <<
                         VeilClient_VERSION_MINOR << "." <<
                         VeilClient_VERSION_PATCH << std::endl;
            std::cout << "FUSE library version: " << FUSE_MAJOR_VERSION << "." <<
                         FUSE_MINOR_VERSION << std::endl;
            return EXIT_SUCCESS;
        }
    }
    catch(VeilException &e)
    {
        std::cerr << "Cannot parse configuration: " << e.what() <<
                     ". Check logs for more details. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

    bool debug = options->get_debug();
    const auto checkCertificate = !options->get_no_check_certificate();

    auto config = std::make_shared<Config>(context);
    context->setConfig(config);

    // proper logger setup
    FLAGS_alsologtostderr = debug;
    FLAGS_logtostderr = debug;
    if(debug)
        FLAGS_stderrthreshold = 2;

    boost::filesystem::path log_path;
    boost::system::error_code ec;

    if(options->is_default_log_dir())
    {
        using namespace boost::filesystem;

        uid_t uid = geteuid();
        std::string userIdent = to_string(uid);
        struct passwd *pw = getpwuid(uid);      // Use UID when getting user name fails
        if(pw) {
            userIdent = pw->pw_name;
        }

        string log_subdir_name = string(argv[0]) + string("_") + userIdent + "_logs";
        log_path = path(options->get_log_dir()) / path( log_subdir_name ).leaf();


        create_directories(log_path, ec);
        if(ec.value() > 0) {
            cerr << "Error: Cannot create log directory: " << log_path.normalize().string() << ". Aborting.";
        }

    }
    else
    {
        log_path = boost::filesystem::path(config->absPathRelToCWD(options->get_log_dir()));
    }


    FLAGS_log_dir = log_path.normalize().string();
    LOG(INFO) << "Setting log dir to: " << log_path.normalize().string();
    google::ShutdownGoogleLogging();
    google::InitGoogleLogging(argv[0]);

    boost::filesystem::permissions(log_path, boost::filesystem::owner_all, ec);
    if(ec.value() > 0) {
        LOG(WARNING) << "Cannot change permissions for log directory (" << log_path.normalize().string() << ") due to: " << ec.message();
    }
    // Logger setup END

    // after logger setup - log version
    LOG(INFO) << "VeilFuse version: " << getVersionString();


    // Iterate over all env variables and save them in Config
    char** env;
    for (env = envp; *env != 0; env++)
    {
        std::vector<std::string> tokens;
        std::string tEnv = std::string(*env);
        boost::split(tokens, tEnv, boost::is_any_of("="));
        if(tokens.size() != 2) // Invalid env variable. Expected format: NAME=VALUE
            continue;

        config->putEnv(tokens[0], tokens[1]);
    }

    // FUSE main:
    struct fuse *fuse;
    struct fuse_chan *ch;
    char *mountpoint;
    int multithreaded;
    int foreground;
    int res;

    struct fuse_args args = options->getFuseArgs();
    res = fuse_parse_cmdline(&args, &mountpoint, &multithreaded, &foreground);
    if (res == -1)
        return EXIT_FAILURE;

    ScopeExit freeMountpoint{[&]{ free(mountpoint); }};

    // Set mount point in global config
    if(mountpoint) {
        config->setMountPoint(string(mountpoint));
        LOG(INFO) << "Using mount point path: " << config->getMountPoint().string();
    }

    auto gsiHandler = std::make_shared<GSIHandler>(context, options->get_debug_gsi());
    std::string token; // TODO:
    GRAdapter grAdapter{context,
                options->get_global_registry_url(),
                options->get_global_registry_port(),
                "/home/kzemek/plgrid/globalregistry/grpca/cacert.pem"}; // TODO:

    if(options->get_authentication() == "certificate")
    {
        // Check proxy certificate
        const auto validationResult = gsiHandler->validateProxyConfig();
        if(!validationResult.first)
        {
            std::cerr << validationResult.second << std::endl;
            std::cerr << "Cannot continue. Aborting" << std::endl;
            return EXIT_FAILURE;
        }
    }
    else if(options->get_authentication() == "token")
    {
        const auto tokenOption = grAdapter.retrieveToken();
        if(tokenOption)
            token = tokenOption.get();
        else
        {
            std::cout << "Authentication Code: ";
            std::string code;
            std::cin >> code;
            token = grAdapter.exchangeCode(code);
        }
        std::cerr << "token: " << token << std::endl;
    }
    else
    {
        std::cerr << "Unknown authentication type: '" <<
                     options->get_authentication() << "'.\n" <<
                     "Cannot continue. Aborting" << std::endl;
        return EXIT_FAILURE;
    }

    ch = fuse_mount(mountpoint, &args);
    if (!ch)
        return EXIT_FAILURE;

    ScopeExit unmountFuse{[&]{ fuse_unmount(mountpoint, ch); }};

    res = fcntl(fuse_chan_fd(ch), F_SETFD, FD_CLOEXEC);
    if (res == -1)
        perror("WARNING: failed to set FD_CLOEXEC on fuse device");

    fuse = fuse_new(ch, &args, &vfs_oper, sizeof(struct fuse_operations), NULL);
    if (fuse == NULL)
        return EXIT_FAILURE;

    ScopeExit destroyFuse{[&]{ fuse_destroy(fuse); }, unmountFuse};

    fuse_set_signal_handlers(fuse_get_session(fuse));
    ScopeExit removeHandlers{[&]{ fuse_remove_signal_handlers(fuse_get_session(fuse)); }};

    const auto certificateData = gsiHandler->getCertData();
    const auto clusterUri = "wss://" + gsiHandler->getClusterHostname() + ":" +
            std::to_string(options->get_cluster_port()) + "/veilclient";

    // Initialize cluster handshake in order to check if everything is ok before becoming daemon
    auto testCommunicator =
            communication::createWebsocketCommunicator(/*dataPoolSize*/ 0,
                                                       /*metaPoolSize*/ 1,
                                                       clusterUri,
                                                       certificateData,
                                                       checkCertificate);

    context->setCommunicator(testCommunicator);

    try
    {
        config->testHandshake();
    }
    catch(CertUnconfirmedException &exception)
    {
        std::string username = exception.getUsername();

        // Prompt user for account confirmation
        std::string userAns;
        do {
            std::cout << CONFIRM_CERTIFICATE_PROMPT(username);
            std::getline(std::cin, userAns);
            std::transform(userAns.begin(), userAns.end(), userAns.begin(), ::tolower);
        } while( std::cin && (userAns.size() == 0 || (userAns[0] != 'y' && userAns[0] != 't' && userAns[0] != 'n')));

        // Exit if input stream was interrupted somehow
        if(!userAns.size())
        {
            std::cerr << std::endl << "Cannot confirm certificate. Aborting." << std::endl;
            return EXIT_FAILURE;
        }

        // Resend handshake request along with account confirmation / rejection
        config->testHandshake(username, userAns[0] == 'y' || userAns[0] == 't');
    }
    catch(communication::InvalidServerCertificate &e)
    {
        cerr << "Server certificate verification failed: " << e.what() <<
                ". Aborting" << endl;

        return EXIT_FAILURE;
    }
    catch(VeilException &exception)
    {
        if(exception.veilError()==NO_USER_FOUND_ERROR)
            cerr << "Cannot find user, remember to login through website before mounting fuse. Aborting" << endl;
        else if(exception.veilError()==NO_CONNECTION_FOR_HANDSHAKE)
            cerr << "Cannot connect to server. Aborting." << endl;
        else
            cerr << "Handshake error. Aborting" << endl;

        return EXIT_FAILURE;
    }

    //cleanup test connections
    context->setCommunicator(nullptr);
    testCommunicator.reset();

    cout << "VeilFS has been successfully mounted in " + string(mountpoint) << endl;

    fuse_remove_signal_handlers(fuse_get_session(fuse));
    res = fuse_daemonize(foreground);
    if (res != -1)
        res = fuse_set_signal_handlers(fuse_get_session(fuse));

    if (res == -1)
        return EXIT_FAILURE;

    // Initialize VeilClient application
    const auto communicator =
            communication::createWebsocketCommunicator(options->get_alive_data_connections_count(),
                                                       options->get_alive_meta_connections_count(),
                                                       clusterUri,
                                                       certificateData,
                                                       checkCertificate);

    context->setCommunicator(communicator);

    // Setup veilhelpers config
    helpers::BufferLimits bufferLimits{options->get_write_buffer_max_size(),
                options->get_read_buffer_max_size(),
                options->get_write_buffer_max_file_size(),
                options->get_read_buffer_max_file_size(),
                options->get_file_buffer_prefered_block_size()};

    // Start all jobSchedulers
    context->addScheduler(std::make_shared<JobScheduler>());
    for(unsigned int i = 1; i < options->get_jobscheduler_threads(); ++i)
        context->addScheduler(std::make_shared<JobScheduler>());

    // Initialize main application object
    auto eventCommunicator = std::make_shared<events::EventCommunicator>(context);
    auto fslogicProxy = std::make_shared<FslogicProxy>(context);
    auto VeilApp = std::make_shared<VeilFS>(mountpoint, context,
                    fslogicProxy,
                    std::make_shared<MetaCache>(context),
                    std::make_shared<LocalStorageManager>(context),
                    std::make_shared<StorageMapper>(context, fslogicProxy),
                    std::make_shared<helpers::StorageHelperFactory>(context->getCommunicator(), bufferLimits),
                    eventCommunicator);
    VeilAppObject = VeilApp;

    // Register remote logWriter for log threshold level updates and start sending loop
    context->getPushListener()->subscribe(std::bind(&logging::RemoteLogWriter::handleThresholdChange,
                                                     logWriter, _1));
    logWriter->run(context->getCommunicator());

    // Enter FUSE loop
    res = multithreaded ? fuse_loop_mt(fuse) : fuse_loop(fuse);
    return res == -1 ? EXIT_FAILURE : EXIT_SUCCESS;
}
