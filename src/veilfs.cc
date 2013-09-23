/**
 * @file veilfs.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "veilfs.h"
#include "fslogicProxy.h"
#include "helpers/storageHelperFactory.h"
#include "metaCache.h"
#include "glog/logging.h"
#include "cstring"
#include "veilErrors.h"
#include <algorithm>

#include <sys/stat.h>

/// Runs FUN on NAME storage helper with constructed with ARGS. Return value is avaiable in 'int sh_return'.
#define SH_RUN(NAME, ARGS, FUN) shared_ptr<helpers::IStorageHelper> ptr = m_shFactory->getStorageHelper(NAME, ARGS); \
                                if(!ptr) { LOG(ERROR) << "storage helper '" << NAME << "' not found"; return -EIO; } \
                                int sh_return = ptr->FUN; 

/// If given veilError does not produce POSIX 0 return code, interrupt execution by returning POSIX error code.
#define RETURN_IF_ERROR(X)  { \
                                int err = translateError(X); \
                                if(err != 0) return err; \
                            }

/// Fetch locationInfo and storageInfo for given file.
/// On success - lInfo and sInfo variables will be set.
/// On error - POSIX error code will be returned, interrupting code execution.
#define GET_LOCATION_INFO(PATH) locationInfo lInfo; \
                                storageInfo sInfo; \
                                try \
                                { \
                                    pair<locationInfo, storageInfo> tmpLoc = m_storageMapper->getLocationInfo(string(PATH), true); \
                                    lInfo = tmpLoc.first; \
                                    sInfo = tmpLoc.second; \
                                } \
                                catch(VeilException e) \
                                { \
                                    LOG(WARNING) << "cannot get file mapping for file: " << string(PATH) << " (error: " << e.what() << ")"; \
                                    return translateError(e.veilError()); \
                                }


using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;


namespace veil {
namespace client {

shared_ptr<Config> VeilFS::m_config;
list<shared_ptr<JobScheduler> > VeilFS::m_jobSchedulers;
shared_ptr<SimpleConnectionPool> VeilFS::m_connectionPool;
ReadWriteLock VeilFS::m_schedulerPoolLock;

VeilFS::VeilFS(string path, shared_ptr<Config> cnf, shared_ptr<JobScheduler> scheduler, 
               shared_ptr<FslogicProxy> fslogic,  shared_ptr<MetaCache> metaCache, 
               shared_ptr<StorageMapper> mapper, shared_ptr<helpers::StorageHelperFactory> sh_factory) : 
    m_fslogic(fslogic),
    m_storageMapper(mapper),
    m_metaCache(metaCache),
    m_shFactory(sh_factory)
{
    if(path.size() > 1 && path[path.size()-1] == '/')
        path = path.substr(0, path.size()-1);
    LOG(INFO) << "setting VFS root dir as: " << string(path);
    m_root = path;

    m_config = cnf;
    VeilFS::addScheduler(scheduler);

    if(m_fslogic) {
        if(VeilFS::getScheduler() && VeilFS::getConfig()) {
            int alive = VeilFS::getConfig()->getInt(ALIVE_CONNECTIONS_COUNT_OPT);
            for(int i = 0; i < alive; ++i) {
                Job pingTask = Job(time(NULL) + i, m_fslogic, ISchedulable::TASK_PING_CLUSTER, VeilFS::getConfig()->getString(ALIVE_CONNECTIONS_COUNT_OPT));
                VeilFS::getScheduler(ISchedulable::TASK_PING_CLUSTER)->addTask(pingTask);
            }
            
        } else 
            LOG(WARNING) << "Connection keep-alive subsystem cannot be started.";
    }
}

VeilFS::~VeilFS()
{
}

void VeilFS::staticDestroy() 
{
    m_config.reset();
    while(m_jobSchedulers.size()) {
        m_jobSchedulers.front().reset();
        m_jobSchedulers.pop_front();
    }
    m_connectionPool.reset();
}

int VeilFS::access(const char *path, int mask)
{
    LOG(INFO) << "FUSE: access(path: " << string(path) << ", mask: " << mask << ")";

    // Always allow accessing file
    // This method should be called in first place. If it is, use 'default_permissions' FUSE flag.
    // Even without this flag, letting this method to return always (int)0 is just OK.
    return 0;
}

int VeilFS::getattr(const char *path, struct stat *statbuf, bool fuse_ctx)
{
    if(fuse_ctx)
        LOG(INFO) << "FUSE: getattr(path: " << string(path) << ", statbuf)";

    FileAttr attr;

    statbuf->st_blocks = 0;
    statbuf->st_nlink = 0; 
    statbuf->st_uid = -1; /// @todo We have to set uid based on usename received from cluster. Currently not supported by cluster.
    statbuf->st_gid = -1; /// @todo same as above
    statbuf->st_size = 0;
    statbuf->st_atime = 0;
    statbuf->st_mtime = 0;
    statbuf->st_ctime = 0;

    if(m_metaCache->getAttr(string(path), statbuf))
        return 0;

    // We do not have storage mapping so we have to comunicate with cluster anyway
    LOG(INFO) << "storage mapping not exists in cache for file: " << string(path);

    if(!m_fslogic->getFileAttr(string(path), &attr))
        return -EIO;

    if(attr.answer() != VOK)
    {
        LOG(WARNING) << "Cluster answer: " << attr.answer();
        return translateError(attr.answer());
    }

    if(attr.type() == "REG" && fuse_ctx) // We'll need storage mapping for regular file
    {
        Job getLocTask = Job(time(NULL), m_storageMapper, ISchedulable::TASK_ASYNC_GET_FILE_LOCATION, string(path));
        VeilFS::getScheduler()->addTask(getLocTask);
    }

    // At this point we have attributes from cluster 

    statbuf->st_mode = attr.mode(); // File type still has to be set, fslogic gives only permissions in mode field

    statbuf->st_atime = attr.atime();
    statbuf->st_mtime = attr.mtime();
    statbuf->st_ctime = attr.ctime();

    if(attr.type() == "DIR")
    {
        statbuf->st_mode |= S_IFDIR;

        // Prefetch "ls" resault
        if(fuse_ctx && VeilFS::getConfig()->getBool(ENABLE_DIR_PREFETCH_OPT)) {
            Job readDirTask = Job(time(NULL), shared_from_this(), ISchedulable::TASK_ASYNC_READDIR, string(path), "0");
            VeilFS::getScheduler()->addTask(readDirTask);
        }
    }
    else if(attr.type() == "LNK")
    {
        statbuf->st_mode |= S_IFLNK;

        // Check cache for validity
        AutoLock lock(m_linkCacheLock, WRITE_LOCK);
        map<string, pair<string, time_t> >::iterator it = m_linkCache.find(string(path));
        if(it != m_linkCache.end() && statbuf->st_mtime > (*it).second.second)
        {
            m_linkCache.erase(it);
        }
    }
    else
    {
        statbuf->st_mode |= S_IFREG;
    }

    m_metaCache->addAttr(string(path), *statbuf);
    return 0;
}

int VeilFS::readlink(const char *path, char *link, size_t size)
{
    LOG(INFO) << "FUSE: readlink(path: " << string(path) << ")";
    string target;

    AutoLock lock(m_linkCacheLock, READ_LOCK);
    map<string, pair<string, time_t> >::const_iterator it = m_linkCache.find(string(path));
    if(it != m_linkCache.end()) {   
        target = (*it).second.first;
    } else {
        pair<string, string> resp = m_fslogic->getLink(string(path));
        target = resp.second;
        RETURN_IF_ERROR(resp.first);

        lock.changeType(WRITE_LOCK);
        m_linkCache[string(path)] = pair<string, time_t>(target, time(NULL));
    }

    if(target.size() == 0) {
        link[0] = 0;
        return 0;
    }

    if(target[0] == '/')
        target = m_root + target;

    int path_size = min(size - 1, target.size()); // truncate path if needed
    memcpy(link, target.c_str(), path_size);
    link[path_size] = 0;

    return 0;
}

int VeilFS::mknod(const char *path, mode_t mode, dev_t dev)
{
    LOG(INFO) << "FUSE: mknod(path: " << string(path) << ", mode: " << mode << ", ...)";
    if(!(mode & S_IFREG))
    {
        LOG(WARNING) << "cannot create non-regular file"; // TODO: or maybe it could be?
        return -EFAULT;
    }

    m_metaCache->clearAttr(string(path));

    FileLocation location;
    if(!m_fslogic->getNewFileLocation(string(path), mode & ALLPERMS, &location))
    {
        LOG(WARNING) << "cannot fetch new file location mapping";
        return -EIO;
    }

    if(location.answer() != VOK)
    {
        LOG(WARNING) << "cannot create node due to cluster error: " << location.answer();
        return translateError(location.answer());
    }

    m_storageMapper->addLocation(string(path), location);
    GET_LOCATION_INFO(path);

    SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_mknod(lInfo.fileId.c_str(), mode, dev));
    return sh_return;
}

int VeilFS::mkdir(const char *path, mode_t mode)
{
    LOG(INFO) << "FUSE: mkdir(path: " << string(path) << ", mode: " << mode << ")";
    m_metaCache->clearAttr(string(path));

    RETURN_IF_ERROR(m_fslogic->createDir(string(path), mode & ALLPERMS));
    
    return 0;
}

int VeilFS::unlink(const char *path)
{
    LOG(INFO) << "FUSE: unlink(path: " << string(path) << ")";
    struct stat statbuf;
    FileAttr attr;
    int isLink = 0;

    if(m_metaCache->getAttr(string(path), &statbuf)) // Check file type in cache
        isLink = S_ISLNK(statbuf.st_mode);
    else if(m_fslogic->getFileAttr(string(path), &attr)) // ... or fetch it from cluster
        isLink = (attr.type() == "LNK");

    m_metaCache->clearAttr(string(path)); // Clear cache

    if(!isLink) 
    {
        GET_LOCATION_INFO(path);
        SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_unlink(lInfo.fileId.c_str()));
        if(sh_return < 0)
            return sh_return;
    }

    RETURN_IF_ERROR(m_fslogic->deleteFile(string(path)));

    return 0;
}

int VeilFS::rmdir(const char *path)
{
    LOG(INFO) << "FUSE: rmdir(path: " << string(path) << ")";
    m_metaCache->clearAttr(string(path));

    RETURN_IF_ERROR(m_fslogic->deleteFile(string(path)));
    return 0;
}

int VeilFS::symlink(const char *to, const char *from)
{
    LOG(INFO) << "FUSE: symlink(path: " << string(from) << ", link: "<< string(to)  <<")";
    string toStr = string(to);
    if(toStr.size() >= m_root.size() && mismatch(m_root.begin(), m_root.end(), toStr.begin()).first == m_root.end()) {
        toStr = toStr.substr(m_root.size());
        if(toStr.size() == 0)
            toStr = "/";
        else if(toStr[0] != '/')
            toStr = string(to);
    }

    LOG(INFO) << "Creating link " << string(from) << "pointing to: " << toStr;

    RETURN_IF_ERROR(m_fslogic->createLink(string(from), toStr)); 
    return 0;
}

int VeilFS::rename(const char *path, const char *newpath)
{
    LOG(INFO) << "FUSE: rename(path: " << string(path) << ", newpath: "<< string(newpath)  <<")";
    RETURN_IF_ERROR(m_fslogic->renameFile(string(path), string(newpath)));

    m_metaCache->clearAttr(string(path));
    return 0;
}

int VeilFS::link(const char *path, const char *newpath)
{
    LOG(INFO) << "FUSE: link(path: " << string(path) << ", newpath: "<< string(newpath)  <<")";
    return -ENOTSUP;
}

// not yet implemeted
int VeilFS::chmod(const char *path, mode_t mode)
{
    LOG(INFO) << "FUSE: chmod(path: " << string(path) << ", mode: "<< mode << ")";
    RETURN_IF_ERROR(m_fslogic->changeFilePerms(string(path), mode & ALLPERMS)); // ALLPERMS = 07777

    m_metaCache->clearAttr(string(path));

    // Chceck is its not regular file
    if(!S_ISREG(mode))
        return 0;

    // If it is, we have to call storage haleper's chmod
    GET_LOCATION_INFO(path);

    SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_chmod(lInfo.fileId.c_str(), mode));
    return sh_return;
}

// not yet implemeted
int VeilFS::chown(const char *path, uid_t uid, gid_t gid)
{
    LOG(INFO) << "FUSE: chown(path: " << string(path) << ", uid: "<< uid << ", gid: " << gid <<")";
    return -EIO;
}

int VeilFS::truncate(const char *path, off_t newSize)
{
    LOG(INFO) << "FUSE: truncate(path: " << string(path) << ", newSize: "<< newSize <<")";
    GET_LOCATION_INFO(path);

    SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_truncate(lInfo.fileId.c_str(), newSize));
    return sh_return;
}

// not yet implemented
int VeilFS::utime(const char *path, struct utimbuf *ubuf)
{
    LOG(INFO) << "FUSE: utime(path: " << string(path) << ", ...)";
    return 0;
}

int VeilFS::open(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: open(path: " << string(path) << ", ...)";
    fileInfo->direct_io = 1;

    GET_LOCATION_INFO(path);

    m_storageMapper->openFile(string(path));
    
    SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_open(lInfo.fileId.c_str(), fileInfo));
    return sh_return;
}

int VeilFS::read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: read(path: " << string(path) << ", size: " << size << ", offset: " << offset << ", ...)";
    GET_LOCATION_INFO(path);

    SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_read(lInfo.fileId.c_str(), buf, size, offset, fileInfo));
    return sh_return;
}

int VeilFS::write(const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: write(path: " << string(path) << ", size: " << size << ", offset: " << offset << ", ...)";
    GET_LOCATION_INFO(path);

    SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_write(lInfo.fileId.c_str(), buf, size, offset, fileInfo));
    return sh_return;
}

// not yet implemented
int VeilFS::statfs(const char *path, struct statvfs *statInfo)
{
    LOG(INFO) << "FUSE: statfs(path: " << string(path) << ", ...)";
    return -EIO;
}

// not yet implemented
int VeilFS::flush(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: flush(path: " << string(path) << ", ...)";
    /* Just a stub.  This method is optional and can safely be left
       unimplemented */

    (void) path;
    (void) fileInfo;
    return 0;
}

int VeilFS::release(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: release(path: " << string(path) << ", ...)";
    m_storageMapper->releaseFile(string(path));

    /// @todo If TASK_SEND_FILE_NOT_USED is scheduled,file mapping has to be removed too. I'm not 100% sure that any of this is needed.
    //VeilFS::getScheduler()->addTask(Job(time(NULL), m_fslogic, ISchedulable::TASK_SEND_FILE_NOT_USED, string(path))); // Uncomment in order to inform cluster that file isnt used anymore

    return 0;
}

// not yet implemented
int VeilFS::fsync(const char *path, int datasync, struct fuse_file_info *fi)
{
    LOG(INFO) << "FUSE: fsync(path: " << string(path) << ", datasync: " << datasync << ")";
    /* Just a stub.  This method is optional and can safely be left
       unimplemented */

    (void) path;
    (void) datasync;
    (void) fi;
    return 0;
}

// not yet implemented, TODO: do we need it?
int VeilFS::opendir(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: opendir(path: " << string(path) << ", ...)";
    return 0;
}

int VeilFS::readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: readdir(path: " << string(path) << ", ..., offset: " << offset << ", ...)";
    std::vector<string> children;

    if(!m_fslogic->getFileChildren(path, DIR_BATCH_SIZE, offset, &children))
    {
        return -EIO;
    }

    for(std::vector<string>::iterator it = children.begin(); it < children.end(); ++it)
    {
        if(VeilFS::getConfig()->getBool(ENABLE_PARALLEL_GETATTR_OPT)) {
            Job readDirTask = Job(time(NULL), shared_from_this(), ISchedulable::TASK_ASYNC_GETATTR, string(path) + (*it));
            VeilFS::getScheduler()->addTask(readDirTask);
        }

        if(filler(buf, it->c_str(), NULL, ++offset))
        {
            LOG(WARNING) << "filler buffer overflow";
            break;
        }
    }


    return 0;
}

// not yet implemented TODO: do we need it?
int VeilFS::releasedir(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: releasedir(path: " << string(path) << ", ...)";
    return 0;
}

// not yet implemented
int VeilFS::fsyncdir(const char *path, int datasync, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: fsyncdir(path: " << string(path) << ", datasync: " << datasync << ", ...)";
    return 0;
}

int VeilFS::setxattr(const char *path, const char *name, const char *value, size_t size, int flags)
{
    return -EIO;
}

int VeilFS::getxattr(const char *path, const char *name, char *value, size_t size)
{
    return -EIO;
}

int VeilFS::listxattr(const char *path, char *list, size_t size)
{
    return -EIO;
}

int VeilFS::removexattr(const char *path, const char *name)
{
    return -EIO;
}

int VeilFS::init(struct fuse_conn_info *conn) {
    LOG(INFO) << "FUSE: init(...)";
    return 0;
}

shared_ptr<JobScheduler> VeilFS::getScheduler(TaskID taskId)
{
    AutoLock lock(m_schedulerPoolLock, WRITE_LOCK);
    shared_ptr<JobScheduler> tmp = m_jobSchedulers.front();

    list<shared_ptr<JobScheduler> >::const_iterator it;
    for(list<shared_ptr<JobScheduler> >::const_iterator it = m_jobSchedulers.begin();
        it != m_jobSchedulers.end(); ++it) 
    {
        if((*it)->hasTask(taskId))
            tmp = (*it);
    }

    return tmp;
}

shared_ptr<Config> VeilFS::getConfig()
{
    return m_config;
}

shared_ptr<SimpleConnectionPool> VeilFS::getConnectionPool()
{
    return m_connectionPool;
}

void VeilFS::addScheduler(shared_ptr<JobScheduler> injected) 
{
    AutoLock lock(m_schedulerPoolLock, WRITE_LOCK);
    m_jobSchedulers.push_back(injected);
}

void VeilFS::setConfig(shared_ptr<Config> injected) 
{
    m_config = injected;
}

void VeilFS::setConnectionPool(shared_ptr<SimpleConnectionPool> injected) 
{
    m_connectionPool = injected;
}

bool VeilFS::runTask(TaskID taskId, string arg0, string arg1, string)
{
    string res;
    struct stat attr;
    ostringstream ss;
    vector<string> children;
    int offset;
    istringstream iss(arg1);

    switch(taskId)
    {
    case TASK_ASYNC_READDIR:
        if(!VeilFS::getConfig()->getBool(ENABLE_ATTR_CACHE_OPT))
            return true;

        iss >> offset;

        if(!m_fslogic->getFileChildren(arg0, DIR_BATCH_SIZE, offset, &children)) {
            return false;
        }

        for(vector<string>::iterator it = children.begin(); it < children.end(); ++it) {
            Job readDirTask = Job(time(NULL), shared_from_this(), ISchedulable::TASK_ASYNC_GETATTR, arg0 + (*it));
            VeilFS::getScheduler()->addTask(readDirTask);
        }

        if(children.size() > 0) {
            ss << offset + children.size();
            string newOffset = ss.str();
            Job readDirTask = Job(time(NULL), shared_from_this(), ISchedulable::TASK_ASYNC_READDIR, arg0, newOffset);
            VeilFS::getScheduler()->addTask(readDirTask);
        }


        return true;
    case TASK_ASYNC_GETATTR:
        if(VeilFS::getConfig()->getBool(ENABLE_ATTR_CACHE_OPT))
            (void) getattr(arg0.c_str(), &attr, false);
        return true;
    default:
        return false;
    }
}

} // namespace client
} // namespace veil