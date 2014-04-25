/**
 * @file veilfs.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEIL_FS_H
#define VEIL_FS_H

#include <errno.h>
#include <fcntl.h>
#include <cstdlib>
#include <unistd.h>
#include <sys/types.h>
#include <sys/xattr.h>
#include <boost/shared_ptr.hpp>
#include "fslogicProxy.h"
#include "config.h"
#include "storageMapper.h"
#include "jobScheduler.h"
#include "metaCache.h"
#include "helpers/storageHelperFactory.h"
#include "simpleConnectionPool.h"
#include "ISchedulable.h"
#include "pushListener.h"
#include "options.h"

#include "events/events.h"

#include <list>
#include <boost/unordered_map.hpp>

/// The name of default global config file
#define GLOBAL_CONFIG_FILE      "veilFuse.conf"

/**
 * How many dirent should be fetch from cluster at once.
 * Note that each opendir syscall will query at least DIR_BATCH_SIZE dirents
 */
#define DIR_BATCH_SIZE  10

namespace veil {
namespace client {

/// Pointer to the Storage Helper's instance
typedef boost::shared_ptr<helpers::IStorageHelper> sh_ptr;

typedef uint64_t helper_cache_idx_t;

/// forward declarations
namespace events{
class EventCommunicator;
}

/**
 * The VeilFS main class.
 * This class contains FUSE all callbacks, so it basically is an heart of the filesystem.
 * Technically VeilFS is an singleton created on programm start and registred in FUSE
 * daemon.
 */
class VeilFS : public ISchedulable {
public:

        static boost::shared_ptr<JobScheduler>  getScheduler(TaskID taskId = TASK_LAST_ID);                 ///< Returns JobScheduler assigned to this object.
        static boost::shared_ptr<Config>  getConfig();                          ///< Returns Config assigned to this object.
        static boost::shared_ptr<SimpleConnectionPool> getConnectionPool();
        static boost::shared_ptr<PushListener>         getPushListener();
        static boost::shared_ptr<Options>  getOptions();                        ///< Returns Options assigned to this object.

        static void addScheduler(boost::shared_ptr<JobScheduler> injected);     ///< Sets JobScheduler object.
        static void setConfig(boost::shared_ptr<Config> injected);              ///< Sets Config object.
        static void setConnectionPool(boost::shared_ptr<SimpleConnectionPool> injected);
        static void setOptions(boost::shared_ptr<Options> injected);

        VeilFS(std::string path, boost::shared_ptr<Config> cnf, boost::shared_ptr<JobScheduler> scheduler,
                boost::shared_ptr<FslogicProxy> fslogic, boost::shared_ptr<MetaCache> metaCache,
                boost::shared_ptr<StorageMapper> mapper, boost::shared_ptr<helpers::StorageHelperFactory> sh_factory,
                boost::shared_ptr<events::EventCommunicator> eventCommunicator); ///< VeilFS constructor.
        virtual ~VeilFS();
        static void staticDestroy();

        int access(const char *path, int mask); /**< *access* FUSE callback. Not implemented yet. */
        int getattr(const char *path, struct stat *statbuf, bool fuse_ctx = true); /**< *getattr* FUSE callback. @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html */
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

        virtual bool runTask(TaskID taskId, std::string arg0, std::string arg1, std::string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask

protected:
        std::string m_root; ///< Filesystem root directory
        uid_t       m_uid;  ///< Filesystem owner's effective uid
        gid_t       m_gid;  ///< Filesystem owner's effective gid
        uid_t       m_ruid;  ///< Filesystem root real uid
        gid_t       m_rgid;  ///< Filesystem root real gid
        uint64_t    m_fh;

        static ReadWriteLock m_schedulerPoolLock;

        boost::shared_ptr<FslogicProxy> m_fslogic;             ///< FslogicProxy instance
        boost::shared_ptr<StorageMapper> m_storageMapper;      ///< StorageMapper instance
        boost::shared_ptr<MetaCache> m_metaCache;              ///< MetaCache instance
        static boost::shared_ptr<Options> m_options;           ///< Options instance
        boost::shared_ptr<helpers::StorageHelperFactory> m_shFactory;   ///< Storage Helpers Factory instance
        boost::shared_ptr<events::EventCommunicator> m_eventCommunicator;
        static std::list<boost::shared_ptr<JobScheduler> > m_jobSchedulers; ///< JobScheduler instances
        static boost::shared_ptr<Config> m_config;             ///< Config instance
        static boost::shared_ptr<SimpleConnectionPool> m_connectionPool;
        static boost::shared_ptr<PushListener> m_pushListener;

        std::map<std::string, std::pair<std::string, time_t> > m_linkCache;         ///< Simple links cache.
        ReadWriteLock m_linkCacheLock;

        boost::unordered_map<helper_cache_idx_t, sh_ptr> m_shCache;         ///< Storage Helpers' cache.
        ReadWriteLock m_shCacheLock;
};

} // namespace client
} // namespace veil

#endif // VEIL_FS_H
