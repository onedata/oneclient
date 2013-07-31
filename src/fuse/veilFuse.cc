/**
 * @file veilFuse.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#define FUSE_USE_VERSION 29

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif

#include <fuse.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <dirent.h>
#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif

#include <boost/filesystem.hpp>
#include <iostream>
using namespace std;


#include "veilfs.hh"
#include "config.hh"

#include "glog/logging.h"

#ifdef __cplusplus
extern "C" {
#endif

void set_rootdir(const char *path) {
    VeilFS::instance()->set_rootdir(path);
}
static int wrap_access(const char *path, int mask) {
    return VeilFS::instance()->access(path, mask);
}
static int wrap_getattr(const char *path, struct stat *statbuf) {
    return VeilFS::instance()->getattr(path, statbuf);
}
static int wrap_readlink(const char *path, char *link, size_t size) {
    return VeilFS::instance()->readlink(path, link, size);
}
static int wrap_mknod(const char *path, mode_t mode, dev_t dev) {
    return VeilFS::instance()->mknod(path, mode, dev);
}
static int wrap_mkdir(const char *path, mode_t mode) {
    return VeilFS::instance()->mkdir(path, mode);
}
static int wrap_unlink(const char *path) {
    return VeilFS::instance()->unlink(path);
}
static int wrap_rmdir(const char *path) {
    return VeilFS::instance()->rmdir(path);
}
static int wrap_symlink(const char *path, const char *link) {
    return VeilFS::instance()->symlink(path, link);
}
static int wrap_rename(const char *path, const char *newpath) {
    return VeilFS::instance()->rename(path, newpath);
}
static int wrap_link(const char *path, const char *newpath) {
    return VeilFS::instance()->link(path, newpath);
}
static int wrap_chmod(const char *path, mode_t mode) {
    return VeilFS::instance()->chmod(path, mode);
}
static int wrap_chown(const char *path, uid_t uid, gid_t gid) {
    return VeilFS::instance()->chown(path, uid, gid);
}
static int wrap_truncate(const char *path, off_t newSize) {
    return VeilFS::instance()->truncate(path, newSize);
}
static int wrap_utime(const char *path, struct utimbuf *ubuf) {
    return VeilFS::instance()->utime(path, ubuf);
}
static int wrap_open(const char *path, struct fuse_file_info *fileInfo) {
    return VeilFS::instance()->open(path, fileInfo);
}
static int wrap_read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo) {
    return VeilFS::instance()->read(path, buf, size, offset, fileInfo);
}
static int wrap_write(const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo) {
    return VeilFS::instance()->write(path, buf, size, offset, fileInfo);
}
static int wrap_statfs(const char *path, struct statvfs *statInfo) {
    return VeilFS::instance()->statfs(path, statInfo);
}
static int wrap_flush(const char *path, struct fuse_file_info *fileInfo) {
    return VeilFS::instance()->flush(path, fileInfo);
}
static int wrap_release(const char *path, struct fuse_file_info *fileInfo) {
    return VeilFS::instance()->release(path, fileInfo);
}
static int wrap_fsync(const char *path, int datasync, struct fuse_file_info *fi) {
    return VeilFS::instance()->fsync(path, datasync, fi);
}
#ifdef HAVE_SETXATTR
static int wrap_setxattr(const char *path, const char *name, const char *value, size_t size, int flags) {
    return VeilFS::instance()->setxattr(path, name, value, size, flags);
}
static int wrap_getxattr(const char *path, const char *name, char *value, size_t size) {
    return VeilFS::instance()->getxattr(path, name, value, size);
}
static int wrap_listxattr(const char *path, char *list, size_t size) {
    return VeilFS::instance()->listxattr(path, list, size);
}
static int wrap_removexattr(const char *path, const char *name) {
    return VeilFS::instance()->removexattr(path, name);
}
#endif // HAVE_SETXATTR
static int wrap_readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo) {
    return VeilFS::instance()->readdir(path, buf, filler, offset, fileInfo);
}
#ifdef HAVE_ADVANCED_DIR
static int wrap_opendir(const char *path, struct fuse_file_info *fileInfo) {
    return VeilFS::instance()->opendir(path, fileInfo);
}
static int wrap_releasedir(const char *path, struct fuse_file_info *fileInfo) {
    return VeilFS::instance()->releasedir(path, fileInfo);
}
static int wrap_fsyncdir(const char *path, int datasync, struct fuse_file_info *fileInfo) {
    return VeilFS::instance()->fsyncdir(path, datasync, fileInfo);
}
static int wrap_init(struct fuse_conn_info *conn) {
    return VeilFS::instance()->init(conn);
}
#endif // HAVE_ADVANCED_DIR

static int vfs_opt_proc(void *data, const char *arg, int key, struct fuse_args *outargs)
{
    if(string(arg).find(CONFIG_ARGV_OPT_NAME) != string::npos)
        return 0;
    return 1;
}

#ifdef __cplusplus
}
#endif

static struct fuse_operations vfs_oper;

static void oper_init() {

    	vfs_oper.getattr	= wrap_getattr;
    	vfs_oper.access		= wrap_access;
    	vfs_oper.readlink	= wrap_readlink;
    	vfs_oper.readdir	= wrap_readdir;
    	vfs_oper.mknod		= wrap_mknod;
    	vfs_oper.mkdir		= wrap_mkdir;
    	vfs_oper.symlink	= wrap_symlink;
    	vfs_oper.unlink		= wrap_unlink;
    	vfs_oper.rmdir		= wrap_rmdir;
    	vfs_oper.rename		= wrap_rename;
    	vfs_oper.link		= wrap_link;
    	vfs_oper.chmod		= wrap_chmod;
    	vfs_oper.chown		= wrap_chown;
    	vfs_oper.truncate	= wrap_truncate;
    #ifdef HAVE_UTIMENSAT
    	vfs_oper.utimens	= wrap_utimens;
    #endif
    	vfs_oper.open		= wrap_open;
    	vfs_oper.read		= wrap_read;
    	vfs_oper.write		= wrap_write;
    	vfs_oper.statfs		= wrap_statfs;
    	vfs_oper.release	= wrap_release;
    	vfs_oper.fsync		= wrap_fsync;
        vfs_oper.utime      = wrap_utime;
        vfs_oper.flush      = wrap_flush;
    #ifdef HAVE_POSIX_FALLOCATE
    	vfs_oper.fallocate	= wrap_fallocate;
    #endif
    #ifdef HAVE_SETXATTR
    	vfs_oper.setxattr	= wrap_setxattr;
    	vfs_oper.getxattr	= wrap_getxattr;
    	vfs_oper.listxattr	= wrap_listxattr;
    	vfs_oper.removexattr= wrap_removexattr;
    #endif
}


static void fuse_init() 
{
    LOG(INFO) << "Intializing fuse callbacks";
    oper_init();
}


int main(int argc, char* argv[]) 
{
    umask(0);

    // Find user config argument
    for(int i = 1; i < argc; ++i)
    {
        if(string(argv[i]).find(CONFIG_ARGV_OPT_NAME) != string::npos)
        {    
            Config::instance().setUserConfigFile(string(argv[i]).substr(string(CONFIG_ARGV_OPT_NAME).size()));
        }
    }

    // Setup config manager and paths
    Config::instance().setGlobalConfigFile(GLOBAL_CONFIG_FILE);
    if(!Config::instance().parseConfig())
    {
        std::cerr << "Cannot load/parse global/user config file. Check logs for more detials. Aborting" << std::endl;
        return 1;
    }

    // logger setup
    if(Config::isSet(LOG_DIR_OPT))
    {
        string log_path = Config::absPathRelToCWD(Config::getValue<string>(LOG_DIR_OPT));
        FLAGS_log_dir = log_path;
        LOG(INFO) << "Setting log dir to: " << log_path;
    }
    google::InitGoogleLogging(argv[0]);
    FLAGS_alsologtostderr = true;

    // Check certificate file
    string certFile = Config::absPathRelToHOME(Config::getValue<string>(PEER_CERTIFICATE_FILE_OPT));
    if( !boost::filesystem::exists( certFile ) )
    {
        std::cerr << "Cannot find peer certificate file: " << certFile << std::endl;
        std::cerr << "Check your configuration. Aborting" << std::endl;
        exit(1);
    }

    // Initialize FUSE
    fuse_init();
    VeilFS::instance()->set_rootdir(argv[1]);
    struct fuse_args args = FUSE_ARGS_INIT(argc, argv);
    fuse_opt_parse(&args, NULL, NULL, vfs_opt_proc);

    // Enforced FUSE options
    fuse_opt_add_arg(&args, "-obig_writes");

    // Start FUSE daemod
    return fuse_main(args.argc, args.argv, &vfs_oper, NULL);
}
