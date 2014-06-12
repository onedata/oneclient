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
#include "logging.h"
#include "cstring"
#include "veilErrors.h"
#include "config.h"
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <algorithm>
#include <boost/filesystem/path.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/functional.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <google/protobuf/descriptor.h>

#include "communication_protocol.pb.h"
#include "fuse_messages.pb.h"
#include "messageBuilder.h"

#include <sys/stat.h>

/// Runs FUN on NAME storage helper with constructed with ARGS. Return value is avaiable in 'int sh_return'.
#define CUSTOM_SH_RUN(PTR, FUN) if(!PTR) { LOG(ERROR) << "Invalid storage helper's pointer!"; return -EIO; } \
                                int sh_return = PTR->FUN; \
                                if(sh_return < 0) LOG(INFO) << "Storage helper returned error: " << sh_return;
#define SH_RUN(NAME, ARGS, FUN) boost::shared_ptr<helpers::IStorageHelper> ptr = m_shFactory->getStorageHelper(NAME, ARGS); \
                                if(!ptr) { LOG(ERROR) << "storage helper '" << NAME << "' not found"; return -EIO; } \
                                CUSTOM_SH_RUN(ptr, FUN)

/// If given veilError does not produce POSIX 0 return code, interrupt execution by returning POSIX error code.
#define RETURN_IF_ERROR(X)  { \
                                int err = translateError(X); \
                                if(err != 0) { LOG(INFO) << "Returning error: " << err; return err; } \
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

/// Get parent path (as string)
#define PARENT(X) filesystem::path(X).branch_path().string()


using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;


namespace veil {
namespace client {

boost::shared_ptr<Config> VeilFS::m_config;
boost::shared_ptr<Options> VeilFS::m_options;
list<boost::shared_ptr<JobScheduler> > VeilFS::m_jobSchedulers;
boost::shared_ptr<SimpleConnectionPool> VeilFS::m_connectionPool;
ReadWriteLock VeilFS::m_schedulerPoolLock;
boost::shared_ptr<PushListener> VeilFS::m_pushListener;

VeilFS::VeilFS(string path, boost::shared_ptr<Config> cnf, boost::shared_ptr<JobScheduler> scheduler,
               boost::shared_ptr<FslogicProxy> fslogic,  boost::shared_ptr<MetaCache> metaCache,
               boost::shared_ptr<LocalStorageManager> sManager, boost::shared_ptr<StorageMapper> mapper,
               boost::shared_ptr<helpers::StorageHelperFactory> sh_factory,
               boost::shared_ptr<events::EventCommunicator> eventCommunicator) :
    m_fh(0),
    m_fslogic(fslogic),
    m_storageMapper(mapper),
    m_metaCache(metaCache),
    m_sManager(sManager),
    m_shFactory(sh_factory),
    m_eventCommunicator(eventCommunicator)
{
    if(path.size() > 1 && path[path.size()-1] == '/')
        path = path.substr(0, path.size()-1);
    LOG(INFO) << "setting VFS root dir as: " << string(path);
    m_root = path;

    m_config = cnf;
    VeilFS::addScheduler(scheduler);

    // Construct new PushListener
    m_pushListener.reset(new PushListener());

    // Update FUSE_ID in current connection pool
    VeilFS::getConnectionPool()->setPushCallback(VeilFS::getConfig()->getFuseID(), boost::bind(&PushListener::onMessage, VeilFS::getPushListener(), _1));

    // Maximum connection count setup
    VeilFS::getConnectionPool()->setPoolSize(SimpleConnectionPool::META_POOL, VeilFS::getOptions()->get_alive_meta_connections_count());
    VeilFS::getConnectionPool()->setPoolSize(SimpleConnectionPool::DATA_POOL, VeilFS::getOptions()->get_alive_data_connections_count());

    // Initialize cluster handshake in order to receive FuseID
    if(VeilFS::getConfig()->getFuseID() == "")
        VeilFS::getConfig()->negotiateFuseID();

    if(m_fslogic) {
        if(VeilFS::getScheduler() && VeilFS::getConfig()) {
            int alive = VeilFS::getOptions()->get_alive_meta_connections_count();
            for(int i = 0; i < alive; ++i) {
                Job pingTask = Job(time(NULL) + i, m_fslogic, ISchedulable::TASK_PING_CLUSTER, boost::lexical_cast<string>(VeilFS::getOptions()->get_alive_meta_connections_count()));
                VeilFS::getScheduler(ISchedulable::TASK_PING_CLUSTER)->addTask(pingTask);
            }

        } else
            LOG(WARNING) << "Connection keep-alive subsystem cannot be started.";
    }

    if(!VeilFS::getOptions()->has_fuse_group_id() && !VeilFS::getConfig()->isEnvSet(string(FUSE_OPT_PREFIX) + string("GROUP_ID"))) {
        if(m_sManager) {
            vector<boost::filesystem::path> mountPoints = m_sManager->getMountPoints();
            vector< pair<int, string> > clientStorageInfo = m_sManager->getClientStorageInfo(mountPoints);
            if(!clientStorageInfo.empty()) {
                m_sManager->sendClientStorageInfo(clientStorageInfo);
            }
        }
    }

    m_uid = geteuid();
    m_gid = getegid();
    // Real IDs should be set real owner's ID of "/" directory by first getattr call
    m_ruid = -1;
    m_rgid = -1;

    if(m_eventCommunicator){
        eventCommunicator->setFslogic(m_fslogic);
        eventCommunicator->setMetaCache(m_metaCache);

        m_eventCommunicator->addStatAfterWritesRule(VeilFS::getOptions()->get_write_bytes_before_stat());
    }

    VeilFS::getPushListener()->subscribe(boost::bind(&events::EventCommunicator::pushMessagesHandler, m_eventCommunicator.get(), _1));
    VeilFS::getScheduler(ISchedulable::TASK_GET_EVENT_PRODUCER_CONFIG)->addTask(Job(time(NULL), m_eventCommunicator, ISchedulable::TASK_GET_EVENT_PRODUCER_CONFIG));
    VeilFS::getScheduler(ISchedulable::TASK_IS_WRITE_ENABLED)->addTask(Job(time(NULL), m_eventCommunicator, ISchedulable::TASK_IS_WRITE_ENABLED));
}

VeilFS::~VeilFS()
{
}

void VeilFS::staticDestroy()
{
    m_options.reset();
    m_config.reset();
    while(m_jobSchedulers.size()) {
        m_jobSchedulers.front().reset();
        m_jobSchedulers.pop_front();
    }
    m_connectionPool.reset();
    m_pushListener.reset();
}

int VeilFS::access(const char *path, int mask)
{
    LOG(INFO) << "FUSE: access(path: " << string(path) << ", mask: " << mask << ")";

    // Always allow accessing file
    // This method should be not called in first place. If it is, use 'default_permissions' FUSE flag.
    // Even without this flag, letting this method to return always (int)0 is just OK.
    return 0;
}

int VeilFS::getattr(const char *path, struct stat *statbuf, bool fuse_ctx)
{
    if(fuse_ctx)
        LOG(INFO) << "FUSE: getattr(path: " << string(path) << ", statbuf)";

    FileAttr attr;

    statbuf->st_blocks = 0;
    statbuf->st_nlink = 1;
    statbuf->st_uid = -1;
    statbuf->st_gid = -1;
    statbuf->st_size = 0;
    statbuf->st_atime = 0;
    statbuf->st_mtime = 0;
    statbuf->st_ctime = 0;

    if(m_metaCache->getAttr(string(path), statbuf))
        return 0;

    // We do not have storage mapping so we have to comunicate with cluster anyway
    LOG(INFO) << "storage mapping not exists in cache for file: " << string(path);

    if(!m_fslogic->getFileAttr(string(path), attr))
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
    statbuf->st_nlink = attr.links();

    statbuf->st_atime = attr.atime();
    statbuf->st_mtime = attr.mtime();
    statbuf->st_ctime = attr.ctime();

    uid_t uid = attr.uid();
    gid_t gid = attr.gid();

    if(string(path) == "/") { // VeilFS root should always belong to FUSE owner
        m_ruid = uid;
        m_rgid = gid;
    }

    // If file belongs to filesystems owner, show FUSE owner ID
    if(m_ruid == uid)
        uid = m_uid;
    if(m_rgid == gid)
        gid = m_gid;

    struct passwd *ownerInfo = getpwnam(attr.uname().c_str()); // Static buffer, do NOT free !
    struct group *groupInfo = getgrnam(attr.gname().c_str());  // Static buffer, do NOT free !

    statbuf->st_uid   = (ownerInfo ? ownerInfo->pw_uid : uid);
    statbuf->st_gid   = (groupInfo ? groupInfo->gr_gid : gid);

    if(attr.type() == "DIR")
    {
        statbuf->st_mode |= S_IFDIR;

        // Prefetch "ls" resault
        if(fuse_ctx && VeilFS::getOptions()->get_enable_dir_prefetch()  && VeilFS::getOptions()->get_enable_attr_cache()) {
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
        statbuf->st_size = attr.size();
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
    if(!m_fslogic->getNewFileLocation(string(path), mode & ALLPERMS, location))
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

    // if file existed before we consider it as a success and we want to apply same actions (chown and sending an acknowledgement)
    if(sh_return == -EEXIST)
        sh_return = 0;

    if(sh_return != 0)
        (void) m_fslogic->deleteFile(string(path));
    else { // File created, now we shall take care of its owner.
        std::vector<std::string> tokens;
        std::string sPath = string(path).substr(1);
        boost::split(tokens, sPath, boost::is_any_of("/"));

        if(tokens.size() > 2 && tokens[0] == "groups") // We are creating file in groups directory
        {
            string groupName = tokens[1];
            struct group *groupInfo = getgrnam(groupName.c_str());  // Static buffer, do NOT free !
            gid_t gid = (groupInfo ? groupInfo->gr_gid : -1);

            // We need to change group owner of this file
            SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_chown(lInfo.fileId.c_str(), -1, gid));
            if(sh_return != 0)
                LOG(ERROR) << "Cannot change group owner of file " << sPath << " to: " << groupName;
        }

        VeilFS::getScheduler()->addTask(Job(time(NULL) + 5, shared_from_this(), TASK_CLEAR_ATTR, PARENT(path))); // Clear cache of parent (possible change of modify time)

        RETURN_IF_ERROR(m_fslogic->sendFileCreatedAck(string(path)));
    }
    return sh_return;
}

int VeilFS::mkdir(const char *path, mode_t mode)
{
    LOG(INFO) << "FUSE: mkdir(path: " << string(path) << ", mode: " << mode << ")";
    m_metaCache->clearAttr(string(path));
    // Clear parent's cache
    m_metaCache->clearAttr(PARENT(path));

    RETURN_IF_ERROR(m_fslogic->createDir(string(path), mode & ALLPERMS));
    VeilFS::getScheduler()->addTask(Job(time(NULL) + 5, shared_from_this(), TASK_CLEAR_ATTR, PARENT(path))); // Clear cache of parent (possible change of modify time)

    boost::shared_ptr<events::Event> mkdirEvent = events::Event::createMkdirEvent(path);
    m_eventCommunicator->processEvent(mkdirEvent);

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
    else if(m_fslogic->getFileAttr(string(path), attr)) // ... or fetch it from cluster
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
    VeilFS::getScheduler()->addTask(Job(time(NULL) + 5, shared_from_this(), TASK_CLEAR_ATTR, PARENT(path))); // Clear cache of parent (possible change of modify time)

    boost::shared_ptr<events::Event> rmEvent = events::Event::createRmEvent(path);
    m_eventCommunicator->processEvent(rmEvent);

    return 0;
}

int VeilFS::rmdir(const char *path)
{
    LOG(INFO) << "FUSE: rmdir(path: " << string(path) << ")";
    m_metaCache->clearAttr(string(path));
    // Clear parent's cache
    m_metaCache->clearAttr(PARENT(path));

    RETURN_IF_ERROR(m_fslogic->deleteFile(string(path)));
    VeilFS::getScheduler()->addTask(Job(time(NULL) + 5, shared_from_this(), TASK_CLEAR_ATTR, PARENT(path))); // Clear cache of parent (possible change of modify time)

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
    VeilFS::getScheduler()->addTask(Job(time(NULL) + 5, shared_from_this(), TASK_CLEAR_ATTR, PARENT(path))); // Clear cache of parent (possible change of modify time)
    VeilFS::getScheduler()->addTask(Job(time(NULL) + 5, shared_from_this(), TASK_CLEAR_ATTR, PARENT(newpath))); // Clear cache of parent (possible change of modify time)

    m_metaCache->clearAttr(string(path));
    return 0;
}

int VeilFS::link(const char *path, const char *newpath)
{
    LOG(INFO) << "FUSE: link(path: " << string(path) << ", newpath: "<< string(newpath)  <<")";
    return -ENOTSUP;
}

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

int VeilFS::chown(const char *path, uid_t uid, gid_t gid)
{
    LOG(INFO) << "FUSE: chown(path: " << string(path) << ", uid: "<< uid << ", gid: " << gid <<")";

    struct passwd *ownerInfo = getpwuid(uid); // Static buffer, do NOT free !
    struct group *groupInfo = getgrgid(gid); // Static buffer, do NOT free !

    string uname = "", gname = "";
    if(ownerInfo)
        uname = ownerInfo->pw_name;
    if(groupInfo)
        gname = groupInfo->gr_name;

    m_metaCache->clearAttr(string(path));

    if((uid_t)-1 != uid)
        RETURN_IF_ERROR(m_fslogic->changeFileOwner(string(path), uid, uname));

    if((gid_t)-1 != gid)
        RETURN_IF_ERROR(m_fslogic->changeFileGroup(string(path), gid, gname));

    return 0;
}

int VeilFS::truncate(const char *path, off_t newSize)
{
    LOG(INFO) << "FUSE: truncate(path: " << string(path) << ", newSize: "<< newSize <<")";
    GET_LOCATION_INFO(path);

    SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_truncate(lInfo.fileId.c_str(), newSize));

    if(sh_return == 0) {
        (void) m_metaCache->updateSize(string(path), newSize);

        Job postTruncateTask = Job(time(NULL), shared_from_this(), TASK_POST_TRUNCATE_ACTIONS, path, utils::toString(newSize));
        VeilFS::getScheduler()->addTask(postTruncateTask);
    }

    return sh_return;
}

int VeilFS::utime(const char *path, struct utimbuf *ubuf)
{
    LOG(INFO) << "FUSE: utime(path: " << string(path) << ", ...)";

    // Update access times in meta cache right away
    (void) m_metaCache->updateTimes(string(path), ubuf->actime, ubuf->modtime);

    VeilFS::getScheduler()->addTask(Job(time(NULL), shared_from_this(), TASK_ASYNC_UPDATE_TIMES, string(path), utils::toString(ubuf->actime), utils::toString(ubuf->modtime)));

    return 0;
}

int VeilFS::open(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: open(path: " << string(path) << ", ...)";
    fileInfo->direct_io = 1;
    fileInfo->fh = ++m_fh;

    GET_LOCATION_INFO(path);

    m_storageMapper->openFile(string(path));

    SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_open(lInfo.fileId.c_str(), fileInfo));

    if(sh_return == 0) {
        AutoLock guard(m_shCacheLock, WRITE_LOCK);
        m_shCache[fileInfo->fh] = ptr;

        time_t atime = 0, mtime = 0;
        mode_t accMode = fileInfo->flags & O_ACCMODE;

        if((accMode == O_WRONLY) || (fileInfo->flags & O_APPEND) || (accMode == O_RDWR))
            mtime = time(NULL);
#ifdef __APPLE__
        if( ( (accMode == O_RDONLY) || (accMode == O_RDWR) ) )
#else
        if( ( (accMode == O_RDONLY) || (accMode == O_RDWR) ) && !(fileInfo->flags & O_NOATIME) )
#endif
            atime = time(NULL);

        if(atime || mtime)
        {
            // Update access times in meta cache right away
            (void) m_metaCache->updateTimes(string(path), atime, mtime);

            VeilFS::getScheduler()->addTask(Job(time(NULL), shared_from_this(), TASK_ASYNC_UPDATE_TIMES, string(path), utils::toString(atime), utils::toString(mtime)));
        }
    }

    return sh_return;
}

int VeilFS::read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo)
{
    //LOG(INFO) << "FUSE: read(path: " << string(path) << ", size: " << size << ", offset: " << offset << ", ...)";
    GET_LOCATION_INFO(path);

    AutoLock guard(m_shCacheLock, READ_LOCK);
    CUSTOM_SH_RUN(m_shCache[fileInfo->fh], sh_read(lInfo.fileId.c_str(), buf, size, offset, fileInfo));

    boost::shared_ptr<events::Event> writeEvent = events::Event::createReadEvent(path, sh_return);
    m_eventCommunicator->processEvent(writeEvent);

    return sh_return;
}

int VeilFS::write(const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo)
{
    //LOG(INFO) << "FUSE: write(path: " << string(path) << ", size: " << size << ", offset: " << offset << ", ...)";

    if(!m_eventCommunicator->isWriteEnabled()){
        LOG(WARNING) << "Attempt to write when write disabled.";
        return -EDQUOT;
    }

    GET_LOCATION_INFO(path);

    AutoLock guard(m_shCacheLock, READ_LOCK);
    CUSTOM_SH_RUN(m_shCache[fileInfo->fh], sh_write(lInfo.fileId.c_str(), buf, size, offset, fileInfo));
    guard.release();

    if(sh_return > 0) { // Update file size in cache
        struct stat buf;
        if(!m_metaCache->getAttr(string(path), &buf))
            buf.st_size = 0;
        if(offset + sh_return > buf.st_size) {
            m_metaCache->updateSize(string(path), offset + sh_return);
        }

        boost::shared_ptr<events::Event> writeEvent = events::Event::createWriteEvent(path, size);
        m_eventCommunicator->processEvent(writeEvent);
    }

    return sh_return;
}

// not yet implemented
int VeilFS::statfs(const char *path, struct statvfs *statInfo)
{
    LOG(INFO) << "FUSE: statfs(path: " << string(path) << ", ...)";

    pair<string, struct statvfs> resp = m_fslogic->getStatFS();
    RETURN_IF_ERROR(resp.first);

    memcpy(statInfo, &resp.second, sizeof(struct statvfs));
    return 0;
}

// not yet implemented
int VeilFS::flush(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: flush(path: " << string(path) << ", ...)";
    GET_LOCATION_INFO(path);

    sh_ptr storage_helper;
    {
        AutoLock guard(m_shCacheLock, READ_LOCK);
        storage_helper = m_shCache[fileInfo->fh];
    }
    CUSTOM_SH_RUN(storage_helper , sh_flush(lInfo.fileId.c_str(), fileInfo));

    VeilFS::getScheduler()->addTask(Job(time(NULL) + 3, shared_from_this(), TASK_CLEAR_ATTR, string(path)));

    return sh_return;
}

int VeilFS::release(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: release(path: " << string(path) << ", ...)";

    /// Remove Storage Helper's pointer from cache
    AutoLock guard(m_shCacheLock, WRITE_LOCK);

    GET_LOCATION_INFO(path);

    CUSTOM_SH_RUN(m_shCache[fileInfo->fh], sh_release(lInfo.fileId.c_str(), fileInfo));

    m_shCache.erase(fileInfo->fh);

    m_storageMapper->releaseFile(string(path));

    return sh_return;
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

int VeilFS::opendir(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: opendir(path: " << string(path) << ", ...)";

    VeilFS::getScheduler()->addTask(Job(time(NULL), shared_from_this(), TASK_ASYNC_UPDATE_TIMES, string(path), utils::toString(time(NULL))));

    return 0;
}

int VeilFS::readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: readdir(path: " << string(path) << ", ..., offset: " << offset << ", ...)";
    vector<string> children;

    if(offset == 0) {
        children.push_back(".");
        children.push_back("..");
    }

    if(!m_fslogic->getFileChildren(path, DIR_BATCH_SIZE, offset >= 2 ? offset - 2 : 0, children))
    {
        return -EIO;
    }

    for(std::vector<string>::iterator it = children.begin(); it < children.end(); ++it)
    {
        if(VeilFS::getOptions()->get_enable_parallel_getattr() && VeilFS::getOptions()->get_enable_attr_cache()) {
            Job readDirTask = Job(time(NULL), shared_from_this(), ISchedulable::TASK_ASYNC_GETATTR, (filesystem::path(path) / (*it)).normalize().string());
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

int VeilFS::releasedir(const char *path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: releasedir(path: " << string(path) << ", ...)";
    return 0;
}

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

boost::shared_ptr<JobScheduler> VeilFS::getScheduler(TaskID taskId)
{
    AutoLock lock(m_schedulerPoolLock, WRITE_LOCK);
    boost::shared_ptr<JobScheduler> front = m_jobSchedulers.front();
    boost::shared_ptr<JobScheduler> tmp = front;

    list<boost::shared_ptr<JobScheduler> >::const_iterator it;
    for(list<boost::shared_ptr<JobScheduler> >::const_iterator it = m_jobSchedulers.begin();
        it != m_jobSchedulers.end(); ++it)
    {
        if((*it)->hasTask(taskId))
            tmp = (*it);
    }

    // Round robin
    m_jobSchedulers.pop_front();
    m_jobSchedulers.push_back(front);

    return tmp;
}

boost::shared_ptr<Config> VeilFS::getConfig()
{
    return m_config;
}

boost::shared_ptr<SimpleConnectionPool> VeilFS::getConnectionPool()
{
    return m_connectionPool;
}

boost::shared_ptr<PushListener> VeilFS::getPushListener()
{
    return m_pushListener;
}

boost::shared_ptr<Options> VeilFS::getOptions()
{
    return m_options;
}

void VeilFS::addScheduler(boost::shared_ptr<JobScheduler> injected)
{
    AutoLock lock(m_schedulerPoolLock, WRITE_LOCK);
    m_jobSchedulers.push_back(injected);
}

void VeilFS::setConfig(boost::shared_ptr<Config> injected)
{
    m_config = injected;
}

void VeilFS::setOptions(boost::shared_ptr<Options> injected)
{
    m_options = injected;
}

void VeilFS::setConnectionPool(boost::shared_ptr<SimpleConnectionPool> injected)
{
    m_connectionPool = injected;
}

bool VeilFS::runTask(TaskID taskId, const string &arg0, const string &arg1, const string &arg2)
{
    struct stat attr;
    vector<string> children;
    time_t currentTime;
    boost::shared_ptr<events::Event> truncateEvent;

    switch(taskId)
    {
    case TASK_ASYNC_READDIR: // arg0 = path, arg1 = offset
        if(!VeilFS::getOptions()->get_enable_attr_cache())
            return true;

        if(!m_fslogic->getFileChildren(arg0, DIR_BATCH_SIZE, utils::fromString<unsigned int>(arg1), children)) {
            return false;
        }

        for(vector<string>::iterator it = children.begin(); it < children.end(); ++it) {
            Job readDirTask = Job(time(NULL), shared_from_this(), ISchedulable::TASK_ASYNC_GETATTR, (filesystem::path(arg0) / (*it)).normalize().string());
            VeilFS::getScheduler()->addTask(readDirTask);
        }

        if(children.size() > 0) {
            Job readDirTask = Job(time(NULL), shared_from_this(), ISchedulable::TASK_ASYNC_READDIR, arg0, utils::toString(utils::fromString<unsigned int>(arg1) + children.size()));
            VeilFS::getScheduler()->addTask(readDirTask);
        }

        return true;

    case TASK_CLEAR_ATTR:
        m_metaCache->clearAttr(arg0);
        return true;

    case TASK_ASYNC_GETATTR:
        if(VeilFS::getOptions()->get_enable_attr_cache())
            getattr(arg0.c_str(), &attr, false);
        return true;

    case TASK_ASYNC_UPDATE_TIMES: // arg0 = path, arg1 = atime, arg2 = mtime
        if(m_fslogic->updateTimes(arg0, utils::fromString<time_t>(arg1), utils::fromString<time_t>(arg2)) == VOK)
            m_metaCache->updateTimes(arg0, utils::fromString<time_t>(arg1), utils::fromString<time_t>(arg2));
        return true;

    case TASK_POST_TRUNCATE_ACTIONS: // arg0 = path, arg1 = newSize
        // we need to statAndUpdatetimes before processing event because we want event to be run with new size value on cluster
        currentTime = time(NULL);
        m_fslogic->updateTimes(arg0, 0, currentTime, currentTime);

        m_metaCache->clearAttr(arg0);
        if(VeilFS::getOptions()->get_enable_attr_cache())
            getattr(arg0.c_str(), &attr, false);

        truncateEvent = events::Event::createTruncateEvent(arg0, utils::fromString<off_t>(arg1));
        m_eventCommunicator->processEvent(truncateEvent);
        return true;

    default:
        return false;
    }
}

} // namespace client
} // namespace veil
