/**
 * @file veilfs.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "veilfs.hh"
#include "fslogicProxy.hh"
#include "helpers/storageHelperFactory.hh"
#include "metaCache.hh"
#include "glog/logging.h"

#include <sys/stat.h>


VeilFS* VeilFS::m_instance = NULL;

/// Runs FUN on NAME storage helper with constructed with ARGS. Return value is avaiable in 'int sh_return'.
#define SH_RUN(NAME, ARGS, FUN) IStorageHelper *ptr = StorageHelperFactory::instance()->getStorageHelper(NAME, ARGS); \
                                if(ptr == NULL) { LOG(ERROR) << "storage helper '" << NAME << "' not found"; return -EIO; } \
                                int sh_return = ptr->FUN; \
                                delete ptr;

/// If given veilError does not produce POSIX 0 return code, interrupt execution by returning POSIX error code.
#define RETURN_IF_ERROR(X)  { \
                                int err = VeilFS::translateError(X); \
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
                                    return VeilFS::translateError(e.veilError()); \
                                }



VeilFS::VeilFS() : 
    m_fslogic(new FslogicProxy()),
    m_storageMapper(new StorageMapper(*m_fslogic)),
    m_jobScheduler(new JobScheduler()),
    m_metaCache(new MetaCache())
{
}

VeilFS::~VeilFS()
{
    delete m_fslogic;
    delete m_storageMapper;
    delete m_jobScheduler;
    delete m_metaCache;
}

void VeilFS::set_rootdir(const char *path)
{
    if(path == NULL)
        return;

    LOG(INFO) << "setting VFS root dir as: " << string(path);
    m_root = path;
}

int VeilFS::access(const char *path, int mask)
{
    LOG(INFO) << "FUSE: access(path: " << string(path) << ", mask: " << mask << ")";

    // Always allow accessing file
    // This method should be called in first place. If it is, use 'default_permissions' FUSE flag.
    // Even without this flag, letting this method to return always (int)0 is just OK.
    return 0;
}

int VeilFS::getattr(const char *path, struct stat *statbuf)
{
    LOG(INFO) << "FUSE: getattr(path: " << string(path) << ", statbuf)";

    FileAttr attr;
    pair<locationInfo, storageInfo> location;

    statbuf->st_blocks = 1;
    statbuf->st_nlink = 0; // Hard links are not supported by VeilCluster ATM
    statbuf->st_uid = -1; /// @todo We have to set uid based on usename received from cluster. Currently not supported by cluster.
    statbuf->st_gid = -1; /// @todo same as above
    statbuf->st_size = 0;
    statbuf->st_atime = 0;
    statbuf->st_mtime = 0;
    statbuf->st_ctime = 0;

    try
    {
        location = m_storageMapper->getLocationInfo(string(path));
        attr.set_type("REG");
    }
    catch(VeilException e)
    {
        if(m_metaCache->getAttr(string(path), statbuf))
            return 0;

        // We do not have storage mapping so we have to comunicate with cluster anyway
        LOG(INFO) << "storage mapping not exists in cache for file: " << string(path);

        if(!m_fslogic->getFileAttr(string(path), &attr))
            return -EIO;

        if(attr.answer() != VOK)
        {
            LOG(WARNING) << "Cluster answer: " << attr.answer();
            return VeilFS::translateError(attr.answer());
        }

        if(attr.type() == "REG") // We'll need storage mapping for regular file
        {
            int err = VeilFS::translateError(m_storageMapper->findLocation(string(path)));
            if(err != 0)
                return err;
        }
    }

    // At this point we have attributes from cluster and storage storage mapping loaded if faile its regular file

    statbuf->st_mode = attr.mode(); // File type still has to be set, fslogic gives only permissions in mode field

    if(attr.type() == "DIR" || attr.type() == "LNK")
    {
        statbuf->st_atime = attr.atime();
        statbuf->st_mtime = attr.atime();
        statbuf->st_ctime = attr.atime();
    }

    if(attr.type() == "DIR")
    {
        statbuf->st_mode |= S_IFDIR;
    }
    else if(attr.type() == "LNK")
    {
        statbuf->st_mode |= S_IFLNK;
    }
    else
    {
        try
        {
             location = m_storageMapper->getLocationInfo(string(path));
        }
        catch(VeilException e)
        {
            LOG(ERROR) << "got attributes from cluster but mapping cannot be found for file '" << string(path) << "', error: " << e.what();
            return -EIO;
        }

        struct stat statbuf_tmp;

        SH_RUN(location.second.storageHelperName, location.second.storageHelperArgs, sh_getattr(location.first.fileId.c_str(), &statbuf_tmp));
        if(sh_return != 0)
        {
            LOG(ERROR) << "storage helper's getattr failed, errno: " << sh_return;
            return sh_return;
        }

        statbuf->st_size = statbuf_tmp.st_size;
        statbuf->st_atime = statbuf_tmp.st_atime;
        statbuf->st_mtime = statbuf_tmp.st_mtime;
        statbuf->st_ctime = statbuf_tmp.st_ctime;
        statbuf->st_blocks = statbuf_tmp.st_blocks;
        statbuf->st_mode =  statbuf_tmp.st_mode;
    }

    m_metaCache->addAttr(string(path), *statbuf);
    return 0;
}

// not yet implemented
int VeilFS::readlink(const char *path, char *link, size_t size)
{
    LOG(INFO) << "FUSE: readlink(path: " << string(path) << ", link: " << string(link) << ", size: " << size << ")";
    return -EIO;
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
        return VeilFS::translateError(location.answer());
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
    m_metaCache->clearAttr(string(path));

    RETURN_IF_ERROR(m_fslogic->deleteFile(string(path)));

    GET_LOCATION_INFO(path);
    SH_RUN(sInfo.storageHelperName, sInfo.storageHelperArgs, sh_unlink(lInfo.fileId.c_str()));

    return sh_return;
}

int VeilFS::rmdir(const char *path)
{
    LOG(INFO) << "FUSE: rmdir(path: " << string(path) << ")";
    m_metaCache->clearAttr(string(path));

    RETURN_IF_ERROR(m_fslogic->deleteFile(string(path)));
    return 0;
}

// not yet implemeted
int VeilFS::symlink(const char *path, const char *link)
{
    LOG(INFO) << "FUSE: symlink(path: " << string(path) << ", link: "<< string(link)  <<")";
    return -EIO;
}

int VeilFS::rename(const char *path, const char *newpath)
{
    LOG(INFO) << "FUSE: rename(path: " << string(path) << ", newpath: "<< string(newpath)  <<")";
    RETURN_IF_ERROR(m_fslogic->renameFile(string(path), string(newpath)));

    m_metaCache->clearAttr(string(path));
    return 0;
}

// not yet implemented
int VeilFS::link(const char *path, const char *newpath)
{
    LOG(INFO) << "FUSE: link(path: " << string(path) << ", newpath: "<< string(newpath)  <<")";
    return -EIO;
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
    return -EIO;
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

JobScheduler* VeilFS::getScheduler()
{
    return VeilFS::instance()->m_jobScheduler;
}

VeilFS* VeilFS::instance()
{
    if(m_instance == NULL)
        m_instance = new VeilFS();

    return m_instance;
}

int VeilFS::translateError(string verr)
{
    if(verr == VOK)
        return 0;
    else if(verr == VENOENT)
        return -ENOENT;
    else if(verr == VEACCES)
        return -EACCES;
    else if(verr == VEEXIST)
        return -EEXIST;
    else if(verr == VEIO)
        return -EIO;
    else
        return -EIO;
}
