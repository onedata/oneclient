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
#include "ISchedulable.h"
#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif

#include <boost/filesystem.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/algorithm/string.hpp>
#include <iostream>

#include "veilfs.h"
#include "config.h"
#include "gsiHandler.h"
#include "glog/logging.h"

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

static int vfs_opt_proc(void *data, const char *arg, int key, struct fuse_args *outargs)
{
    if(string(arg).find(CONFIG_ARGV_OPT_NAME) != string::npos)
        return 0;
    if(string(arg) == "-debug")
        return 0;
    return 1;
}

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

int main(int argc, char* argv[], char* envp[]) 
{
    // Turn off logging for a while
    google::InitGoogleLogging(argv[0]);
    FLAGS_alsologtostderr = false;
    FLAGS_logtostderr = false;
    FLAGS_stderrthreshold = 3;

    // Initialize FUSE
    umask(0);

    fuse_init();
    struct fuse_args args = FUSE_ARGS_INIT(argc, argv);
    fuse_opt_parse(&args, NULL, NULL, vfs_opt_proc);

    // Enforced FUSE options
    fuse_opt_add_arg(&args, "-obig_writes");

    bool debug = false; // Assume silent mode
    bool showVersionOnly = false;

    boost::shared_ptr<Config> config(new Config());
    VeilFS::setConfig(config);

    // Find user config argument
    for(int i = 1; i < argc; ++i)
    {
        if(string(argv[i]).find(CONFIG_ARGV_OPT_NAME) != string::npos)
        {
            config->setUserConfigFile(string(argv[i]).substr(string(CONFIG_ARGV_OPT_NAME).size()));
        }

        if(string(argv[i]) == "-d") // FUSE's debug flag
            debug = true;

        if(string(argv[i]) == "-debug") // GSI Handler's debug flag
            gsi::debug = true;

        if(string(argv[i]) == "--version" || string(argv[i]) == "-V") {
            cout << "VeilFuse version: " 
                 << VeilClient_VERSION_MAJOR << "."
                 << VeilClient_VERSION_MINOR << "."
                 << VeilClient_VERSION_PATCH << endl;
            showVersionOnly = true;
        } else if(string(argv[i]) == "--help" || string(argv[i]) == "-h") {
            showVersionOnly = true;
        }
    }

    // Setup config manager and paths
    config->setGlobalConfigFile(GLOBAL_CONFIG_FILE);
    if(!config->parseConfig())
    {
        std::cerr << "Cannot load/parse global/user config file. Check logs for more detials. Aborting" << std::endl;
        exit(1);
    }

    // proper logger setup
    if(config->isSet(LOG_DIR_OPT))
    {
        string log_path = Config::absPathRelToCWD(config->getString(LOG_DIR_OPT));
        if(log_path != "/tmp") {
            FLAGS_log_dir = log_path;
            LOG(INFO) << "Setting log dir to: " << log_path;
            // Restart Google Loggler in order ot reload log_dir path
            google::ShutdownGoogleLogging();
            google::InitGoogleLogging(argv[0]);
        }
    }
    FLAGS_alsologtostderr = debug;
    FLAGS_logtostderr = debug;
    if(debug)
        FLAGS_stderrthreshold = 2;

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

    res = fuse_parse_cmdline(&args, &mountpoint, &multithreaded, &foreground);
    if (res == -1)
        exit(1);

    if(showVersionOnly) { // Exit after showing full version info or help banner
        exit(EXIT_SUCCESS);
    }
    
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
	boost::shared_ptr<SimpleConnectionPool> testPool(new SimpleConnectionPool(gsi::getClusterHostname(), config->getInt(CLUSTER_PORT_OPT), boost::bind(&gsi::getCertInfo)));
	VeilFS::setConnectionPool(testPool);
	try{
		config->testHandshake();
	}
	catch (VeilException &exception) {
		if(exception.veilError()==NO_USER_FOUND_ERROR)
			cerr << "Cannot find user, remember to login through website before mounting fuse. Aborting" << endl;
		else if(exception.veilError()==NO_CONNECTION_FOR_HANDSHAKE)
			cerr << "Cannot connect to server. Aborting" << endl;
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
        new SimpleConnectionPool(gsi::getClusterHostname(), config->getInt(CLUSTER_PORT_OPT), boost::bind(&gsi::getCertInfo))));
    
    // Setup veilhelpers config
    veil::helpers::config::setConnectionPool(VeilFS::getConnectionPool());
    veil::helpers::config::buffers::writeBufferGlobalSizeLimit  = config->getInt(WRITE_BUFFER_MAX_SIZE_OPT);
    veil::helpers::config::buffers::readBufferGlobalSizeLimit   = config->getInt(READ_BUFFER_MAX_SIZE_OPT);
    veil::helpers::config::buffers::writeBufferPerFileSizeLimit = config->getInt(WRITE_BUFFER_MAX_FILE_SIZE_OPT);
    veil::helpers::config::buffers::readBufferPerFileSizeLimit  = config->getInt(READ_BUFFER_MAX_FILE_SIZE_OPT);
    veil::helpers::config::buffers::preferedBlockSize           = config->getInt(FILE_BUFFER_PREFERED_BLOCK_SIZE_OPT);

    // Start all jobSchedulers
    for(int i = 1; i < config->getInt(JOBSCHEDULER_THREADS_OPT); ++i)
        VeilFS::addScheduler(boost::shared_ptr<JobScheduler>(new JobScheduler()));

    // Initialize main application object
    VeilFS * veilfs = new VeilFS(mountpoint, config,
                        boost::shared_ptr<JobScheduler>(new JobScheduler()),
                        boost::shared_ptr<FslogicProxy>(new FslogicProxy()),
                        boost::shared_ptr<MetaCache>(new MetaCache()),
                        boost::shared_ptr<StorageMapper>(new StorageMapper(boost::shared_ptr<FslogicProxy>(new FslogicProxy()))),
                        boost::shared_ptr<helpers::StorageHelperFactory>(new helpers::StorageHelperFactory()),
                        boost::shared_ptr<EventCommunicator>(new EventCommunicator()));
    VeilAppObject.reset(veilfs);
    cout << "------- Veilfuse bazinga --------" << endl;
    VeilFS::getPushListener()->subscribe(boost::bind(&VeilFS::eventsNeededHandler, veilfs, _1));
    VeilFS::getScheduler(ISchedulable::TASK_GET_EVENT_PRODUCER_CONFIG)->addTask(Job(time(NULL) + 1, boost::shared_ptr<VeilFS> (VeilAppObject), ISchedulable::TASK_GET_EVENT_PRODUCER_CONFIG));

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
