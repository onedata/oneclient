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

#include <fuse.h>
#include <fuse/fuse_opt.h>
#include <fuse/fuse_lowlevel.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include "ISchedulable.h"
#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif

#include <boost/filesystem.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/smart_ptr/make_shared.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#include <iostream>

#include "veilfs.h"
#include "config.h"
#include "gsiHandler.h"
#include "logging.h"
#include "options.h"

#include "fslogicProxy.h"

using namespace std;
using namespace boost;
using namespace veil;
using namespace veil::client;
using boost::filesystem::path;

/// Main  application object (filesystem state)
boost::shared_ptr<VeilFS> VeilAppObject;

#ifdef __cplusplus
extern "C" {
#endif

static int wrap_access(const char *path, int mask) {
    return VeilAppObject->access(path, mask);
}
static int wrap_getattr(const char *path, struct stat *statbuf) {
    return VeilAppObject->getattr(path, statbuf);
}
static int wrap_readlink(const char *path, char *link, size_t size) {
    return VeilAppObject->readlink(path, link, size);
}
static int wrap_mknod(const char *path, mode_t mode, dev_t dev) {
    return VeilAppObject->mknod(path, mode, dev);
}
static int wrap_mkdir(const char *path, mode_t mode) {
    return VeilAppObject->mkdir(path, mode);
}
static int wrap_unlink(const char *path) {
    return VeilAppObject->unlink(path);
}
static int wrap_rmdir(const char *path) {
    return VeilAppObject->rmdir(path);
}
static int wrap_symlink(const char *path, const char *link) {
    return VeilAppObject->symlink(path, link);
}
static int wrap_rename(const char *path, const char *newpath) {
    return VeilAppObject->rename(path, newpath);
}
static int wrap_link(const char *path, const char *newpath) {
    return VeilAppObject->link(path, newpath);
}
static int wrap_chmod(const char *path, mode_t mode) {
    return VeilAppObject->chmod(path, mode);
}
static int wrap_chown(const char *path, uid_t uid, gid_t gid) {
    return VeilAppObject->chown(path, uid, gid);
}
static int wrap_truncate(const char *path, off_t newSize) {
    return VeilAppObject->truncate(path, newSize);
}
static int wrap_utime(const char *path, struct utimbuf *ubuf) {
    return VeilAppObject->utime(path, ubuf);
}
static int wrap_open(const char *path, struct fuse_file_info *fileInfo) {
    return VeilAppObject->open(path, fileInfo);
}
static int wrap_read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo) {
    return VeilAppObject->read(path, buf, size, offset, fileInfo);
}
static int wrap_write(const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo) {
    return VeilAppObject->write(path, buf, size, offset, fileInfo);
}
static int wrap_statfs(const char *path, struct statvfs *statInfo) {
    return VeilAppObject->statfs(path, statInfo);
}
static int wrap_flush(const char *path, struct fuse_file_info *fileInfo) {
    return VeilAppObject->flush(path, fileInfo);
}
static int wrap_release(const char *path, struct fuse_file_info *fileInfo) {
    return VeilAppObject->release(path, fileInfo);
}
static int wrap_fsync(const char *path, int datasync, struct fuse_file_info *fi) {
    return VeilAppObject->fsync(path, datasync, fi);
}
#ifdef HAVE_SETXATTR
static int wrap_setxattr(const char *path, const char *name, const char *value, size_t size, int flags) {
    return VeilAppObject->setxattr(path, name, value, size, flags);
}
static int wrap_getxattr(const char *path, const char *name, char *value, size_t size) {
    return VeilAppObject->getxattr(path, name, value, size);
}
static int wrap_listxattr(const char *path, char *list, size_t size) {
    return VeilAppObject->listxattr(path, list, size);
}
static int wrap_removexattr(const char *path, const char *name) {
    return VeilAppObject->removexattr(path, name);
}
#endif // HAVE_SETXATTR
static int wrap_readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo) {
    return VeilAppObject->readdir(path, buf, filler, offset, fileInfo);
}
static int wrap_opendir(const char *path, struct fuse_file_info *fileInfo) {
    return VeilAppObject->opendir(path, fileInfo);
}
static int wrap_releasedir(const char *path, struct fuse_file_info *fileInfo) {
    return VeilAppObject->releasedir(path, fileInfo);
}
static int wrap_fsyncdir(const char *path, int datasync, struct fuse_file_info *fileInfo) {
    return VeilAppObject->fsyncdir(path, datasync, fileInfo);
}
// static int wrap_init(struct fuse_conn_info *conn) {
//     return VeilAppObject->init(conn);
// }


#ifdef __cplusplus
}
#endif

static struct fuse_operations vfs_oper;

static void oper_init() {

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
}


static void fuse_init()
{
    //LOG(INFO) << "Intializing fuse callbacks";
    oper_init();
}

static std::string getVersionString()
{
    std::stringstream ss;
    ss << VeilClient_VERSION_MAJOR << "."
        << VeilClient_VERSION_MINOR << "."
        << VeilClient_VERSION_PATCH;
    return ss.str();
}

int main(int argc, char* argv[], char* envp[])
{
    // Turn off logging for a while
    google::InitGoogleLogging(argv[0]);
    FLAGS_alsologtostderr = false;
    FLAGS_logtostderr = false;
    FLAGS_stderrthreshold = 3;

    // Set up a remote logger
    auto logWriter = boost::make_shared<logging::RemoteLogWriter>();
    logging::setLogSinks(new logging::RemoteLogSink{logWriter},
                         new logging::RemoteLogSink{logWriter, protocol::logging::LDEBUG});

    // Initialize FUSE
    umask(0);
    fuse_init();

    boost::shared_ptr<Options> options = boost::make_shared<Options>();
    VeilFS::setOptions(options);
    try
    {
        // On --version (-V), --help (-h) prints and exits with success
        options->parseConfigs(argc, argv);
    }
    catch(VeilException &e)
    {
        std::cerr << "Cannot parse configuration: " << e.what() <<
                     ". Check logs for more details. Aborting" << std::endl;
        exit(EXIT_FAILURE);
    }

    bool debug = options->get_debug();
    gsi::debug = options->get_debug_gsi();
    helpers::config::checkCertificate.store(!options->get_no_check_certificate());

    boost::shared_ptr<Config> config(new Config());
    VeilFS::setConfig(config);

    // proper logger setup
    FLAGS_alsologtostderr = debug;
    FLAGS_logtostderr = debug;
    if(debug)
        FLAGS_stderrthreshold = 2;
    
    filesystem::path log_path;
    system::error_code ec;
    
    if(options->is_default_log_dir()) {
        using namespace boost::filesystem;
        
        string log_subdir_name = string(argv[0]) + string("_") + string(cuserid(NULL)) + "_logs";
        log_path = path(options->get_log_dir()) / path( log_subdir_name ).leaf();
        
        
        create_directories(log_path, ec);
        if(ec.value() > 0) {
            cerr << "Error: Cannot create log directory: " << log_path.normalize().string() << ". Aborting.";
        }
        
    } else {
        log_path = filesystem::path( Config::absPathRelToCWD(options->get_log_dir()) );
    }
    
    
    
    FLAGS_log_dir = log_path.normalize().string();
    LOG(INFO) << "Setting log dir to: " << log_path.normalize().string();
    google::ShutdownGoogleLogging();
    google::InitGoogleLogging(argv[0]);
 
    filesystem::permissions(log_path, filesystem::owner_all, ec);
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
        
        Config::putEnv(tokens[0], tokens[1]);
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
        exit(1);

    // Set mount point in global config
    if(mountpoint) {
        Config::setMountPoint(string(mountpoint));
        LOG(INFO) << "Using mount point path: " << Config::getMountPoint().string();
    }

    // Check proxy certificate
    if(!gsi::validateProxyConfig())
    {
        std::cerr << "Cannot continue. Aborting" << std::endl;
        exit(1);
    }

    ch = fuse_mount(mountpoint, &args);
    if (!ch)
        exit(1);

    res = fcntl(fuse_chan_fd(ch), F_SETFD, FD_CLOEXEC);
    if (res == -1)
        perror("WARNING: failed to set FD_CLOEXEC on fuse device");

    fuse = fuse_new(ch, &args, &vfs_oper, sizeof(struct fuse_operations), NULL);
    if (fuse == NULL) {
        fuse_unmount(mountpoint, ch);
        exit(1);
    }

    fuse_set_signal_handlers(fuse_get_session(fuse));

    // Initialize cluster handshake in order to check if everything is ok before becoming daemon
    boost::shared_ptr<SimpleConnectionPool> testPool(new SimpleConnectionPool(gsi::getClusterHostname(), options->get_cluster_port(), boost::bind(&gsi::getCertInfo)));
    VeilFS::setConnectionPool(testPool);
    try{
        config->testHandshake();
    }
    catch (VeilException &exception) {
        if(exception.veilError()==NO_USER_FOUND_ERROR)
            cerr << "Cannot find user, remember to login through website before mounting fuse. Aborting" << endl;
        else if(exception.veilError()==NO_CONNECTION_FOR_HANDSHAKE)
        {
            if(testPool->getLastError() == error::SERVER_CERT_VERIFICATION_FAILED)
                cerr << "Server certificate verification failed. Aborting" << endl;
            else
                cerr << "Cannot connect to server. Aborting." << endl;
        }
        else
            cerr << "Handshake error. Aborting" << endl;
        fuse_unmount(mountpoint, ch);
        exit(1);
    }

    //cleanup test connections
    VeilFS::setConnectionPool(boost::shared_ptr<SimpleConnectionPool> ());
    testPool.reset();

    cout << "VeilFS has been successfully mounted in " + string(mountpoint) << endl;

    fuse_remove_signal_handlers(fuse_get_session(fuse));
    res = fuse_daemonize(foreground);
    if (res != -1)
        res = fuse_set_signal_handlers(fuse_get_session(fuse));

    if (res == -1) {
        fuse_unmount(mountpoint, ch);
        fuse_destroy(fuse);
        exit(1);
    }

    // Initialize VeilClient application
    VeilFS::setConnectionPool(boost::shared_ptr<SimpleConnectionPool> (
        new SimpleConnectionPool(gsi::getClusterHostname(), options->get_cluster_port(), boost::bind(&gsi::getCertInfo))));

    // Setup veilhelpers config
    veil::helpers::config::setConnectionPool(VeilFS::getConnectionPool());
    veil::helpers::config::buffers::writeBufferGlobalSizeLimit  = options->get_write_buffer_max_size();
    veil::helpers::config::buffers::readBufferGlobalSizeLimit   = options->get_read_buffer_max_size();
    veil::helpers::config::buffers::writeBufferPerFileSizeLimit = options->get_write_buffer_max_file_size();
    veil::helpers::config::buffers::readBufferPerFileSizeLimit  = options->get_read_buffer_max_file_size();
    veil::helpers::config::buffers::preferedBlockSize           = options->get_file_buffer_prefered_block_size();

    // Start all jobSchedulers
    for(unsigned int i = 1; i < options->get_jobscheduler_threads(); ++i)
        VeilFS::addScheduler(boost::shared_ptr<JobScheduler>(new JobScheduler()));

    // Initialize main application object
    boost::shared_ptr<events::EventCommunicator> eventCommunicator (new events::EventCommunicator());
    VeilAppObject.reset(new VeilFS(mountpoint, config,
                        boost::shared_ptr<JobScheduler>(new JobScheduler()),
                        boost::shared_ptr<FslogicProxy>(new FslogicProxy()),
                        boost::shared_ptr<MetaCache>(new MetaCache()),
                        boost::shared_ptr<LocalStorageManager>(new LocalStorageManager()),
                        boost::shared_ptr<StorageMapper>(new StorageMapper(boost::shared_ptr<FslogicProxy>(new FslogicProxy()))),
                        boost::shared_ptr<helpers::StorageHelperFactory>(new helpers::StorageHelperFactory()),
                        eventCommunicator));

    // Register remote logWriter for log threshold level updates and start sending loop
    VeilFS::getPushListener()->subscribe(boost::bind(&logging::RemoteLogWriter::handleThresholdChange,
                                                     logWriter, _1));
    logWriter->run();

    // Enter FUSE loop
    if (multithreaded)
        res = fuse_loop_mt(fuse);
    else
        res = fuse_loop(fuse);

    if (res == -1)
        res = 1;
    else
        res = 0;


    // Cleanup
    fuse_remove_signal_handlers(fuse_get_session(fuse));
    fuse_unmount(mountpoint, ch);
    fuse_destroy(fuse);
    free(mountpoint);

    return res;

}
