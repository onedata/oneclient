/**
 * @file fsImpl.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */
#include "fsImpl.h"

#include "communication_protocol.pb.h"
#include "communication/communicator.h"
#include "config.h"
#include "context.h"
#include "fslogicProxy.h"
#include "fuse_messages.pb.h"
#include "helpers/storageHelperFactory.h"
#include "localStorageManager.h"
#include "logging.h"
#include "metaCache.h"
#include "options.h"
#include "pushListener.h"
#include "scheduler.h"
#include "storageMapper.h"
#include "oneErrors.h"
#include "oneException.h"
#include "events/eventManager.h"

#include <grp.h>
#include <pwd.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/lexical_cast.hpp>
#include <google/protobuf/descriptor.h>

#include <algorithm>
#include <cstring>
#include <functional>
#include <tuple>

using namespace one::clproto::fuse_messages;
using namespace std::literals::chrono_literals;
using SH = one::helpers::IStorageHelper;

namespace {

inline int translateAndLogError(const std::string &errStr)
{
    int err = one::translateError(errStr);
    if (err != 0)
        LOG(INFO) << "Returning error: " << err;

    return err;
}

/// Get parent path (as string)
inline std::string parent(const boost::filesystem::path &p)
{
    return p.branch_path().string();
}

} // namespace

namespace one {
namespace client {

FsImpl::FsImpl(std::string path, std::shared_ptr<Context> context,
               std::shared_ptr<FslogicProxy> fslogic,  std::shared_ptr<MetaCache> metaCache,
               std::shared_ptr<LocalStorageManager> sManager,
               std::shared_ptr<helpers::StorageHelperFactory> sh_factory,
               std::shared_ptr<events::EventManager> eventManager) :
    m_fh(0),
    m_fslogic(std::move(fslogic)),
    m_metaCache(std::move(metaCache)),
    m_sManager(std::move(sManager)),
    m_shFactory(std::move(sh_factory)),
    m_eventManager{std::move(eventManager)},
    m_context{std::move(context)}
{
    if(path.size() > 1 && path[path.size()-1] == '/')
        path = path.substr(0, path.size()-1);
    LOG(INFO) << "setting VFS root dir as: " << path;
    m_root = path;

    // Construct new PushListener
    auto pushListener = std::make_shared<PushListener>(m_context);
    m_context->setPushListener(pushListener);

    // Update FUSE_ID in current connection pool
    m_context->getCommunicator()->setFuseId(m_context->getConfig()->getFuseID());
    m_context->getCommunicator()->setupPushChannels(std::bind(&PushListener::onMessage, pushListener, std::placeholders::_1));

    // Initialize cluster handshake in order to receive FuseID
    if(m_context->getConfig()->getFuseID() == "")
        m_context->getConfig()->negotiateFuseID();

    if(m_fslogic) {
        if(m_context->scheduler() && m_context->getConfig()) {
            int alive = m_context->getOptions()->get_alive_meta_connections_count();
            for(int i = 0; i < alive; ++i) {
                m_context->scheduler()->schedule(
                            std::chrono::seconds{i}, &FslogicProxy::pingCluster,
                            m_fslogic);
            }

        } else
            LOG(WARNING) << "Connection keep-alive subsystem cannot be started.";
    }

    if(!m_context->getOptions()->has_fuse_group_id() && !m_context->getConfig()->isEnvSet(std::string(FUSE_OPT_PREFIX) + std::string("GROUP_ID"))) {
        if(m_sManager) {
            std::vector<boost::filesystem::path> mountPoints = m_sManager->getMountPoints();
            std::vector<std::pair<int, std::string>> clientStorageInfo = m_sManager->getClientStorageInfo(mountPoints);
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

    m_context->getPushListener()->subscribe(&MetaCache::handleNotification, m_metaCache);
}

FsImpl::~FsImpl()
{
}

int FsImpl::access(const std::string &path, int mask)
{
    LOG(INFO) << "FUSE: access(path: " << path << ", mask: " << mask << ")";

    // Always allow accessing file
    // This method should be not called in first place. If it is, use 'default_permissions' FUSE flag.
    // Even without this flag, letting this method to return always (int)0 is just OK.
    return 0;
}

int FsImpl::getattr(const std::string &path, struct stat *statbuf, bool fuse_ctx)
{
    if(fuse_ctx)
        LOG(INFO) << "FUSE: getattr(path: " << path << ", statbuf)";

    // Initialize statbuf in case getattr is called for its side-effects.
    struct stat scopedStatbuf;
    if(!statbuf)
        statbuf = &scopedStatbuf;

    FileAttr attr;

    statbuf->st_blocks = 0;
    statbuf->st_nlink = 1;
    statbuf->st_uid = -1;
    statbuf->st_gid = -1;
    statbuf->st_size = 0;
    statbuf->st_atime = 0;
    statbuf->st_mtime = 0;
    statbuf->st_ctime = 0;

    m_context->getStorageMapper()->resetHelperOverride(path);

    if(!m_metaCache->getAttr(path, statbuf))
    {
        // We do not have storage mapping so we have to comunicate with cluster anyway
        LOG(INFO) << "storage mapping not exists in cache for file: " << path;

        if(!m_fslogic->getFileAttr(path, attr))
            return -EIO;

        if(attr.answer() != VOK)
        {
            LOG(WARNING) << "Cluster answer: " << attr.answer();
            return translateError(attr.answer());
        }

        if(attr.type() == "REG" && fuse_ctx) // We'll need storage mapping for regular file
        {
            m_context->scheduler()->post(
                        std::bind(&StorageMapper::asyncGetFileLocation, m_context->getStorageMapper(), path));
        }

        // At this point we have attributes from cluster

        std::string fileUUID;
        std::tie(fileUUID, *statbuf) = m_metaCache->parseFileAttr(attr);

        uid_t uid = attr.uid();
        gid_t gid = attr.gid();

        if(path == "/") { // FsImpl root should always belong to FUSE owner
            m_ruid = uid;
            m_rgid = gid;
        }

        if(attr.type() == "DIR")
        {
            // Prefetch "ls" result
            if(fuse_ctx && m_context->getOptions()->get_enable_dir_prefetch() && m_context->getOptions()->get_enable_attr_cache())
            {
                m_context->scheduler()->post(
                            &FsImpl::asyncReaddir, shared_from_this(), path, 0);
            }
        }
        else if(attr.type() == "LNK")
        {
            // Check cache for validity
            if(const auto &cached = m_linkCache.get(path))
                if(statbuf->st_mtime > cached.get().second)
                    m_linkCache.take(path);
        }

        m_metaCache->addAttr(fileUUID, path, *statbuf);
    }

    return 0;
}

int FsImpl::readlink(const std::string &path, char *link, size_t size)
{
    LOG(INFO) << "FUSE: readlink(path: " << path << ")";
    std::string target;

    if(const auto &cached = m_linkCache.get(path)) {
        target = cached.get().first;
    } else {
        std::pair<std::string, std::string> resp = m_fslogic->getLink(path);
        target = resp.second;
        if(int err = translateAndLogError(resp.first))
            return err;

        m_linkCache.set(path, std::make_pair(target, time(nullptr)));
    }

    if(target.size() == 0) {
        link[0] = 0;
        return 0;
    }

    if(target[0] == '/')
        target = m_root + target;

    int path_size = std::min(size - 1, target.size()); // truncate path if needed
    memcpy(link, target.c_str(), path_size);
    link[path_size] = 0;

    return 0;
}

int FsImpl::mknod(const std::string &path, mode_t mode, dev_t dev)
{
    LOG(INFO) << "FUSE: mknod(path: " << path << ", mode: " << mode << ", ...)";
    if(!(mode & S_IFREG))
    {
        LOG(WARNING) << "cannot create non-regular file"; // TODO: or maybe it could be?
        return -EFAULT;
    }

    m_metaCache->clearAttr(path);

    FileLocation location;
    if(!m_fslogic->getNewFileLocation(path, mode & ALLPERMS, location, needsForceClusterProxy(parent(path))))
    {
        LOG(WARNING) << "cannot fetch new file location mapping";
        return -EIO;
    }

    if(location.answer() != VOK)
    {
        LOG(WARNING) << "cannot create node due to cluster error: " << location.answer();
        return translateError(location.answer());
    }

    m_context->getStorageMapper()->addLocation(path, location);

    LocationInfo lInfo;
    StorageInfo sInfo;
    std::tie(lInfo, sInfo) = getLocationInfo(path, true, false);

    int shReturn;
    std::tie(shReturn, std::ignore) =
        shRun(&SH::sh_mknod, sInfo, lInfo.fileId.c_str(), mode, dev);

    // if file existed before we consider it as a success and we want to apply same actions (chown and sending an acknowledgement)
    if(shReturn == -EEXIST)
        shReturn = 0;

    if(shReturn != 0)
        (void) m_fslogic->deleteFile(path);
    else { // File created, now we shall take care of its owner.
        std::vector<std::string> tokens;
        std::string sPath = path.substr(1);
        boost::split(tokens, sPath, boost::is_any_of("/"));

        if(tokens.size() > 2 && tokens[0] == "groups") // We are creating file in groups directory
        {
            std::string groupName = tokens[1];
            struct group *groupInfo = getgrnam(groupName.c_str());  // Static buffer, do NOT free !
            gid_t gid = (groupInfo ? groupInfo->gr_gid : -1);

            // We need to change group owner of this file
            std::tie(shReturn, std::ignore) =
                shRun(&SH::sh_chown, sInfo, lInfo.fileId.c_str(), -1, gid);

            if(shReturn != 0)
                LOG(ERROR) << "Cannot change group owner of file " << sPath << " to: " << groupName;
        }

        scheduleClearAttr(parent(path));

        if(int err = translateAndLogError(m_fslogic->sendFileCreatedAck(path)))
           return err;
    }
    return shReturn;
}

int FsImpl::mkdir(const std::string &path, mode_t mode)
{
    LOG(INFO) << "FUSE: mkdir(path: " << path << ", mode: " << mode << ")";
    m_metaCache->clearAttr(path);
    // Clear parent's cache
    m_metaCache->clearAttr(parent(path).c_str());

    if(int err = translateAndLogError(m_fslogic->createDir(path, mode & ALLPERMS)))
        return err;

    scheduleClearAttr(parent(path));

    return 0;
}

int FsImpl::unlink(const std::string &path)
{
    LOG(INFO) << "FUSE: unlink(path: " << path << ")";
    struct stat statbuf;
    FileAttr attr;
    int isLink = 0;

    int attrStatus = getattr(path, &statbuf, false);
    isLink = S_ISLNK(statbuf.st_mode);

    m_metaCache->clearAttr(path); // Clear cache

    if(!isLink)
    {
        m_context->getStorageMapper()->clearMappings(path);
        LocationInfo lInfo;
        StorageInfo sInfo;
        std::tie(lInfo, sInfo) = getLocationInfo(
            path, true, needsForceClusterProxy(parent(path)) || attrStatus ||
                            !m_metaCache->canUseDefaultPermissions(
                                statbuf)); // Get file location from cluster

        if(int err = translateAndLogError(m_fslogic->deleteFile(path)))
            return err;

        int shReturn;
        std::tie(shReturn, std::ignore) =
            shRun(&SH::sh_unlink, sInfo, lInfo.fileId.c_str());

        if(shReturn < 0)
            return shReturn;
    } else
    {
        if(int err = translateAndLogError(m_fslogic->deleteFile(path)))
            return err;
    }

    scheduleClearAttr(parent(path));

    return 0;
}

int FsImpl::rmdir(const std::string &path)
{
    LOG(INFO) << "FUSE: rmdir(path: " << path << ")";
    m_metaCache->clearAttr(path);
    // Clear parent's cache
    m_metaCache->clearAttr(parent(path).c_str());

    if(int err = translateAndLogError(m_fslogic->deleteFile(path)))
        return err;

    scheduleClearAttr(parent(path));

    return 0;
}

int FsImpl::symlink(const std::string &to, const std::string &from)
{
    LOG(INFO) << "FUSE: symlink(path: " << from << ", link: " << to << ")";

    std::string toStr = to;
    if(toStr.size() >= m_root.size() && mismatch(m_root.begin(), m_root.end(), toStr.begin()).first == m_root.end()) {
        toStr = toStr.substr(m_root.size());
        if(toStr.size() == 0)
            toStr = "/";
        else if(toStr[0] != '/')
            toStr = to;
    }

    LOG(INFO) << "Creating link " << from << "pointing to: " << toStr;

    return translateAndLogError(m_fslogic->createLink(from, toStr));
}

int FsImpl::rename(const std::string &path, const std::string &newpath)
{
    LOG(INFO) << "FUSE: rename(path: " << path << ", newpath: " << newpath << ")";

    if(int err = translateAndLogError(m_fslogic->renameFile(path, newpath)))
        return err;

    scheduleClearAttr(parent(path));
    scheduleClearAttr(parent(newpath));
    m_metaCache->clearAttr(path);
    return 0;
}

int FsImpl::link(const std::string &path, const std::string &newpath)
{
    LOG(INFO) << "FUSE: link(path: " << path << ", newpath: " << newpath << ")";
    return -ENOTSUP;
}

int FsImpl::chmod(const std::string &path, mode_t mode)
{
    LOG(INFO) << "FUSE: chmod(path: " << path << ", mode: " << mode << ")";
    if(int err = translateAndLogError(m_fslogic->changeFilePerms(path, mode & ALLPERMS))) // ALLPERMS = 07777
        return err;

    m_metaCache->clearAttr(path);

    // Chceck is its not regular file
    if(!S_ISREG(mode))
        return 0;

    // If it is, we have to call storage haleper's chmod
    LocationInfo lInfo;
    StorageInfo sInfo;
    std::tie(lInfo, sInfo) = getLocationInfo(path, true, needsForceClusterProxy(path));

    int shReturn;
    std::tie(shReturn, std::ignore) =
        shRun(&SH::sh_chmod, sInfo, lInfo.fileId.c_str(), mode);

    return shReturn;
}

int FsImpl::chown(const std::string &path, uid_t uid, gid_t gid)
{
    LOG(INFO) << "FUSE: chown(path: " << path << ", uid: " << uid << ", gid: " << gid << ")";

    struct passwd *ownerInfo = getpwuid(uid); // Static buffer, do NOT free !
    struct group *groupInfo = getgrgid(gid); // Static buffer, do NOT free !

    std::string uname = "", gname = "";
    if(ownerInfo)
        uname = ownerInfo->pw_name;
    if(groupInfo)
        gname = groupInfo->gr_name;

    m_metaCache->clearAttr(path);

    if((uid_t)-1 != uid)
        if(int err = translateAndLogError(m_fslogic->changeFileOwner(path, uid, uname)))
            return err;

    if((gid_t)-1 != gid)
        if(int err = translateAndLogError(m_fslogic->changeFileGroup(path, gid, gname)))
            return err;

    return 0;
}

int FsImpl::truncate(const std::string &path, off_t newSize)
{
    LOG(INFO) << "FUSE: truncate(path: " << path << ", newSize: " << newSize << ")";

    LocationInfo lInfo;
    StorageInfo sInfo;
    std::tie(lInfo, sInfo) = getLocationInfo(path, true, needsForceClusterProxy(path));

    int shReturn;
    std::tie(shReturn, std::ignore) =
        shRun(&SH::sh_truncate, sInfo, lInfo.fileId.c_str(), newSize);

    if(shReturn == 0) {
        m_metaCache->updateSize(path, newSize);
        m_eventManager->createTruncateEvent(path, newSize)->emit();
    }

    return shReturn;
}

int FsImpl::utime(const std::string &path, struct utimbuf *ubuf)
{
    LOG(INFO) << "FUSE: utime(path: " << path << ", ...)";

    // Update access times in meta cache right away
    (void) m_metaCache->updateTimes(path, ubuf->actime, ubuf->modtime);

    m_context->scheduler()->post(&FsImpl::updateTimes, shared_from_this(), path,
                                 ubuf->actime, ubuf->modtime);

    return 0;
}

int FsImpl::open(const std::string &path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: open(path: " << path << ", ...)";
    fileInfo->direct_io = 1;
    fileInfo->fh = ++m_fh;
    mode_t accMode = fileInfo->flags & O_ACCMODE;

    if(m_context->getOptions()->get_enable_permission_checking()){
        std::string openMode = UNSPECIFIED_MODE;
        if(accMode == O_RDWR)
            openMode = RDWR_MODE;
        else if(accMode== O_RDONLY)
            openMode = READ_MODE;
        else if(accMode == O_WRONLY)
            openMode = WRITE_MODE;
        std::string status;
        if(VOK != (status =  m_context->getStorageMapper()->findLocation(path, openMode)))
            return translateError(status);
    }

    LocationInfo lInfo;
    StorageInfo sInfo;
    std::tie(lInfo, sInfo) = getLocationInfo(path, true, needsForceClusterProxy(path));

    m_context->getStorageMapper()->openFile(path);

    int shReturn;
    std::shared_ptr<SH> ptr;
    std::tie(shReturn, ptr) =
        shRun(&SH::sh_open, sInfo, lInfo.fileId.c_str(), fileInfo);

    if(shReturn == 0) {
        m_shCache.set(fileInfo->fh, ptr);

        time_t atime = 0, mtime = 0;

        if((accMode == O_WRONLY) || (fileInfo->flags & O_APPEND) || (accMode == O_RDWR))
            mtime = time(nullptr);
#ifdef __APPLE__
        if( ( (accMode == O_RDONLY) || (accMode == O_RDWR) ) )
#else
        if( ( (accMode == O_RDONLY) || (accMode == O_RDWR) ) && !(fileInfo->flags & O_NOATIME) )
#endif
            atime = time(nullptr);

        if(atime || mtime)
        {
            // Update access times in meta cache right away
            m_metaCache->updateTimes(path, atime, mtime);
            m_context->scheduler()->post(&FsImpl::updateTimes, shared_from_this(),
                                         path, atime, mtime);
        }
    }

    return shReturn;
}

int FsImpl::read(const std::string &path, char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo)
{
    //LOG(INFO) << "FUSE: read(path: " << path << ", size: " << size << ", offset: " << offset << ", ...)";

    struct stat statbuf;
    if (!m_metaCache->getAttr(path, &statbuf))
        return -EIO;

    const auto fileSize = statbuf.st_size;
    if (offset >= fileSize)
        return 0;

    LocationInfo lInfo;
    StorageInfo sInfo;

    // make sure that we have anything to read
    std::tie(lInfo, sInfo) = getLocationInfo(path, false, false);

    const auto wantedBlock = boost::icl::discrete_interval<off_t>(offset, offset + size);
    const auto availableBlock = lInfo.blocks.lower_bound(wantedBlock);

    if (availableBlock == lInfo.blocks.end() || !boost::icl::contains(*availableBlock, wantedBlock))
    {
        m_fslogic->requestFileBlock(path, offset, size);
    }

    if (availableBlock == lInfo.blocks.end() || !boost::icl::contains(*availableBlock, offset))
    {
        if (!m_context->getStorageMapper()->waitForBlock(path, offset))
            return -EIO;
    }

    std::tie(lInfo, sInfo) = getLocationInfo(path, false, false);
    const auto block = lInfo.blocks.find(offset);
    if (block == lInfo.blocks.end())
        return -EIO;

    const auto toRead = std::min<size_t>(size, boost::icl::last(*block) - offset + 1);

    auto sh = m_shCache.get(fileInfo->fh).get();
    int shReturn = customSHRun(&SH::sh_read, sh, lInfo.fileId.c_str(), buf, toRead, offset, fileInfo);

    m_eventManager->createReadEvent(path, offset, shReturn)->emit();

    return shReturn;
}

int FsImpl::write(const std::string &path, const std::string &buf, size_t size, off_t offset, struct fuse_file_info *fileInfo)
{
    //LOG(INFO) << "FUSE: write(path: " << path << ", size: " << size << ", offset: " << offset << ", ...)";

    LocationInfo lInfo;
    StorageInfo sInfo;
    std::tie(lInfo, sInfo) = getLocationInfo(path, false, false);

    auto sh = m_shCache.get(fileInfo->fh).get();
    int shReturn = customSHRun(&SH::sh_write, sh, lInfo.fileId.c_str(), buf.c_str(), size, offset, fileInfo);

    if(shReturn > 0) { // Update file size in cache
        struct stat buf;
        if(!m_metaCache->getAttr(path, &buf))
            buf.st_size = 0;
        if(offset + shReturn > buf.st_size) {
            m_metaCache->updateSize(path, offset + shReturn);
            m_eventManager->createWriteEvent(path, offset, shReturn, offset + shReturn)->emit();
        } else {
            m_eventManager->createWriteEvent(path, offset, shReturn, buf.st_size)->emit();
        }
    }

    return shReturn;
}

// not yet implemented
int FsImpl::statfs(const std::string &path, struct statvfs *statInfo)
{
    LOG(INFO) << "FUSE: statfs(path: " << path << ", ...)";

    std::pair<std::string, struct statvfs> resp = m_fslogic->getStatFS();
    if(int err = translateAndLogError(resp.first))
        return err;

    memcpy(statInfo, &resp.second, sizeof(struct statvfs));
    return 0;
}

// not yet implemented
int FsImpl::flush(const std::string &path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: flush(path: " << path << ", ...)";

    LocationInfo lInfo;
    StorageInfo sInfo;
    std::tie(lInfo, sInfo) = getLocationInfo(path, false, false);

    auto sh = m_shCache.get(fileInfo->fh).get();
    int shReturn = customSHRun(&SH::sh_flush, sh, lInfo.fileId.c_str(), fileInfo);

    scheduleClearAttr(path);

    return shReturn;
}

int FsImpl::release(const std::string &path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: release(path: " << path << ", ...)";

    LocationInfo lInfo;
    StorageInfo sInfo;
    std::tie(lInfo, sInfo) = getLocationInfo(path, false, false);

    /// Remove Storage Helper's pointer from cache
    auto sh = m_shCache.take(fileInfo->fh);
    int shReturn = customSHRun(&SH::sh_release, sh, lInfo.fileId.c_str(), fileInfo);

    m_context->getStorageMapper()->releaseFile(path);

    return shReturn;
}

// not yet implemented
int FsImpl::fsync(const std::string &path, int datasync, struct fuse_file_info *fi)
{
    LOG(INFO) << "FUSE: fsync(path: " << path << ", datasync: " << datasync << ")";
    /* Just a stub.  This method is optional and can safely be left
       unimplemented */

    (void) path;
    (void) datasync;
    (void) fi;
    return 0;
}

int FsImpl::opendir(const std::string &path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: opendir(path: " << path << ", ...)";

    m_context->scheduler()->post(&FsImpl::updateTimes, shared_from_this(),
                                 path, time(nullptr), 0);

    return 0;
}

int FsImpl::readdir(const std::string &path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: readdir(path: " << path << ", ..., offset: " << offset << ", ...)";
    std::vector<std::string> children;

    if(offset == 0) {
        children.push_back(".");
        children.push_back("..");
    }

    if(!m_fslogic->getFileChildren(path, DIR_BATCH_SIZE, offset >= 2 ? offset - 2 : 0, children))
    {
        return -EIO;
    }

    for(auto it = children.begin(); it < children.end(); ++it)
    {
        if(m_context->getOptions()->get_enable_parallel_getattr() && m_context->getOptions()->get_enable_attr_cache())
        {
            const auto arg = (boost::filesystem::path(path) / (*it)).normalize().string();
            m_context->scheduler()->post(&FsImpl::getattr, shared_from_this(),
                                         arg.c_str(), nullptr, false);
        }

        if(filler(buf, it->c_str(), nullptr, ++offset))
        {
            LOG(WARNING) << "filler buffer overflow";
            break;
        }
    }


    return 0;
}

int FsImpl::releasedir(const std::string &path, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: releasedir(path: " << path << ", ...)";
    return 0;
}

int FsImpl::fsyncdir(const std::string &path, int datasync, struct fuse_file_info *fileInfo)
{
    LOG(INFO) << "FUSE: fsyncdir(path: " << path << ", datasync: " << datasync << ", ...)";
    return 0;
}

int FsImpl::setxattr(const std::string &path, const std::string &name, const std::string &value, size_t size, int flags)
{
    return -EIO;
}

int FsImpl::getxattr(const std::string &path, const std::string &name, char *value, size_t size)
{
    return -EIO;
}

int FsImpl::listxattr(const std::string &path, char *list, size_t size)
{
    return -EIO;
}

int FsImpl::removexattr(const std::string &path, const std::string &name)
{
    return -EIO;
}

int FsImpl::init(struct fuse_conn_info *conn) {
    LOG(INFO) << "FUSE: init(...)";
    return 0;
}


bool FsImpl::needsForceClusterProxy(const std::string &path)
{
    struct stat attrs;
    auto attrsStatus = getattr(path.c_str(), &attrs, false);
    auto filePermissions = attrs.st_mode & (S_IRWXU | S_IRWXG | S_IRWXO);
    return attrsStatus || (filePermissions == 0) || !m_metaCache->canUseDefaultPermissions(attrs);
}

std::pair<LocationInfo, StorageInfo>
FsImpl::getLocationInfo(const std::string &path, const bool useCluster,
                        const bool forceProxy)
{
    try {
        return m_context->getStorageMapper()->getLocationInfo(path, useCluster,
                                                              forceProxy);
    }
    catch (OneException e) {
        LOG(WARNING) << "cannot get file mapping for file: " << path
                     << " (error: " << e.what() << ")";
        throw;
    }
}

void FsImpl::scheduleClearAttr(const std::string &path)
{
    // Clear cache of parent (possible change of modify time)
    m_context->scheduler()->schedule(5s, &MetaCache::clearAttr, m_metaCache, path);
}

void FsImpl::asyncReaddir(const std::string &path, const size_t offset)
{
    if(!m_context->getOptions()->get_enable_attr_cache())
        return;

    std::vector<std::string> children;
    if(!m_fslogic->getFileChildren(path, DIR_BATCH_SIZE, offset, children))
        return;

    for(const auto &name: children)
    {
        const auto arg = (boost::filesystem::path(path) / name).normalize().string();
        m_context->scheduler()->post(&FsImpl::getattr, shared_from_this(),
                                     arg.c_str(), nullptr, false);
    }

    if(!children.empty())
    {
        m_context->scheduler()->post(&FsImpl::asyncReaddir, shared_from_this(),
                                     path, offset + children.size());
    }
}

void FsImpl::updateTimes(const std::string &path, const time_t atime, const time_t mtime)
{
    if(m_fslogic->updateTimes(path, atime, mtime) == VOK)
        m_metaCache->updateTimes(path, atime, mtime);
}

template<typename key, typename value>
boost::optional<value&> FsImpl::ThreadsafeCache<key, value>::get(const key id)
{
    std::shared_lock<std::shared_timed_mutex> lock{m_cacheMutex};
    const auto it = m_cache.find(id);

    if(it == m_cache.end())
        return {};

    return {it->second};
}

template<typename key, typename value>
void FsImpl::ThreadsafeCache<key, value>::set(const key id, value val)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_cacheMutex};
    m_cache[id] = std::move(val);
}

template<typename key, typename value>
value FsImpl::ThreadsafeCache<key, value>::take(const key id)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_cacheMutex};
    const auto it = m_cache.find(id);
    auto sh = std::move(it->second);
    m_cache.erase(it);
    return sh;
}

template <typename... Args1, typename... Args2>
int FsImpl::customSHRun(int (one::helpers::IStorageHelper::*fun)(Args1...),
                        const std::shared_ptr<helpers::IStorageHelper> &ptr,
                        Args2... args)
{
    if (!ptr) {
        LOG(ERROR) << "Invalid storage helper's pointer!";
        return -EIO;
    }
    int shReturn = ((*ptr).*fun)(std::forward<Args2>(args)...);
    if (shReturn < 0)
        LOG(INFO) << "Storage helper returned error: " << shReturn;
    return shReturn;
}

template <typename... Args1, typename... Args2>
std::pair<int, std::shared_ptr<one::helpers::IStorageHelper>>
FsImpl::shRun(int (one::helpers::IStorageHelper::*fun)(Args1...),
              const StorageInfo &sInfo, Args2... args)
{
    auto ptr = m_shFactory->getStorageHelper(sInfo.storageHelperName,
                                             sInfo.storageHelperArgs);
    if (!ptr) {
        LOG(ERROR) << "Storage helper '" << sInfo.storageHelperName
                   << "' not found";
        return {-EIO, nullptr};
    }
    return {customSHRun(fun, ptr, std::forward<Args2>(args)...),
            std::move(ptr)};
}

} // namespace client
} // namespace one
