/**
 * @file veilfs.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEIL_FS_H
#define VEIL_FS_H

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <fuse.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/xattr.h>
#include <boost/shared_ptr.hpp>
#include "fslogicProxy.hh"
#include "config.hh"
#include "storageMapper.hh"
#include "jobScheduler.hh"
#include "metaCache.hh"
#include "helpers/storageHelperFactory.hh"

using namespace boost;

/// VeilClient error codes
#define VOK         "ok"
#define VENOENT     "enoent"
#define VEACCES     "eacces"
#define VEEXIST     "eexist"
#define VEIO        "eio"
#define VENOTSUP    "enotsup"

/// The name of default global config file
#define GLOBAL_CONFIG_FILE      "veilFuse.conf"

/// The command line pattern used to find user config path ARGV
#define CONFIG_ARGV_OPT_NAME    "--config="

/**
 * How many dirent should be fetch from cluster at once.
 * Note that each opendir syscall will query at least DIR_BATCH_SIZE dirents
 */
#define DIR_BATCH_SIZE  10

/**
 * The VeilFS main class.
 * This class contains FUSE all callbacks, so it basically is an heart of the filesystem.
 * Technically VeilFS is an singleton created on programm start and registred in FUSE
 * daemon.
 */
class VeilFS {
public:
        /**
         * errno translator.
         * Translates internal VeilCLient error codes (strings) to
         * POSIX error codes. If given string is not valid,
         * EIO is returned.
         * @param verr literal name of POSIX error code
         * @return POSIX error code multiplied by -1
         */
        static int translateError(string verr);
        static shared_ptr<JobScheduler>  getScheduler();                 ///< Returns JobScheduler assigned to this object.
        static shared_ptr<Config>  getConfig();                          ///< Returns Config assigned to this object.
        
        static void setScheduler(shared_ptr<JobScheduler> injected);     ///< Sets JobScheduler object.

        VeilFS(string path, shared_ptr<Config> cnf, shared_ptr<JobScheduler> scheduler, 
               shared_ptr<FslogicProxy> fslogic,  shared_ptr<MetaCache> metaCache, 
               shared_ptr<StorageMapper> mapper, shared_ptr<StorageHelperFactory> sh_factory); ///< VeilFS constructor.
        virtual ~VeilFS();
        static void staticDestroy();

        int access(const char *path, int mask); /**< *access* FUSE callback. Not implemented yet. */
        int getattr(const char *path, struct stat *statbuf); /**< *getattr* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int readlink(const char *path, char *link, size_t size); /**< *readlink* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int mknod(const char *path, mode_t mode, dev_t dev); /**< *mknod* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int mkdir(const char *path, mode_t mode); /**< mkdir FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int unlink(const char *path); /**< *unlink* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int rmdir(const char *path); /**< *rmdir* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int symlink(const char *path, const char *link); /**< *symlink* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int rename(const char *path, const char *newpath); /**< *rename* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int link(const char *path, const char *newpath); /**< *link* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int chmod(const char *path, mode_t mode); /**< *chmod* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int chown(const char *path, uid_t uid, gid_t gid); /**< *chown* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int truncate(const char *path, off_t newSize); /**< *truncate* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int utime(const char *path, struct utimbuf *ubuf); /**< *utime* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int open(const char *path, struct fuse_file_info *fileInfo); /**< *open* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo); /**< *read* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int write(const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo); /**< *write* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int statfs(const char *path, struct statvfs *statInfo); /**< *statfs* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int flush(const char *path, struct fuse_file_info *fileInfo); /**< *flush* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int release(const char *path, struct fuse_file_info *fileInfo); /**< *release* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int fsync(const char *path, int datasync, struct fuse_file_info *fi); /**< *fsync* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int setxattr(const char *path, const char *name, const char *value, size_t size, int flags); /**< *setxattr* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int getxattr(const char *path, const char *name, char *value, size_t size); /**< *getxattr* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int listxattr(const char *path, char *list, size_t size); /**< *listxattr* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int removexattr(const char *path, const char *name); /**< *removexattr* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int opendir(const char *path, struct fuse_file_info *fileInfo); /**< *opendir* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo); /**< *readdir* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int releasedir(const char *path, struct fuse_file_info *fileInfo); /**< *releasedir* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int fsyncdir(const char *path, int datasync, struct fuse_file_info *fileInfo); /**< *fsyncdir* FUSE callback. Not implemented yet. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
        int init(struct fuse_conn_info *conn); /**< *init* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */


private:
        const char *m_root; ///< Filesystem root directory

        shared_ptr<FslogicProxy> m_fslogic;             ///< FslogicProxy instance
        shared_ptr<StorageMapper> m_storageMapper;      ///< StorageMapper instance
        static shared_ptr<JobScheduler> m_jobScheduler; ///< JobScheduler instance
        shared_ptr<MetaCache> m_metaCache;              ///< MetaCache instance
        static shared_ptr<Config> m_config;             ///< Config instance
        shared_ptr<StorageHelperFactory> m_shFactory;   ///< Storage Helpers Factory instance


};

#endif // VEIL_FS_H
