/**
 * @file virtualFsRegistry.h
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FSLOGIC_VIRTUAL_FS_REGISTRY_H
#define ONECLIENT_FSLOGIC_VIRTUAL_FS_REGISTRY_H

#include "cache/helpersCache.h"
#include "cache/metadataCache.h"
#include "helpers/storageHelper.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileBlock.h"
#include "messages/fuse/fileLocation.h"

namespace one {
namespace client {
namespace fslogic {
class FsLogic;
}
namespace virtualfs {

using one::client::cache::HelpersCacheBase;
using one::client::cache::MetadataCache;
using one::client::fslogic::FsLogic;
using one::helpers::FileHandle;
using one::helpers::FileHandlePtr;
using one::helpers::Params;
using one::helpers::StorageHelper;
using one::helpers::Timeout;
using one::messages::fuse::FileAttr;
using one::messages::fuse::FileBlock;
using one::messages::fuse::FileLocation;

/**
 * @brief Adapter for virtual files
 *
 * This class defines an interface for adapters of the existing filesystem
 * structure. It allows to implement adapters which modify the structure
 * of the filesystem tree or provide non-existing files by custom names.
 */
class VirtualFsAdapter : public StorageHelper,
                         public std::enable_shared_from_this<VirtualFsAdapter> {
public:
    VirtualFsAdapter(FsLogic &fslogic, const folly::fbstring &storageId)
        : m_fsLogic{fslogic}
        , m_storageId{storageId}
    {
    }

    virtual ~VirtualFsAdapter() = default;

    folly::fbstring storageId() const { return m_storageId; }

    FsLogic &fsLogic() { return m_fsLogic; }

    virtual bool match(const folly::fbstring &direntry) = 0;

    virtual folly::fbstring effectiveName(const folly::fbstring &direntry) = 0;

    virtual void readdir(const folly::fbvector<folly::fbstring> &entries,
        std::shared_ptr<const FileAttr> attr, MetadataCache &metadataCache) = 0;

    virtual std::shared_ptr<FileLocation> getFileLocation(
        std::shared_ptr<FileAttr> attr)
    {
        auto uuid = attr->uuid();
        auto loc = std::make_shared<FileLocation>();
        loc->putBlock(0, *attr->size(),
            FileBlock{m_storageId.toStdString(), uuid.toStdString()});
        return loc;
    }

    virtual folly::fbstring name() const = 0;

    virtual folly::Future<std::shared_ptr<FileHandle>> open(
        const folly::fbstring &fileId, const int flags,
        const Params &openParams) = 0;

    std::shared_ptr<FileHandle> getFileHandle(const folly::fbstring &fileId)
    {
        return m_fileHandles.at(fileId);
    }

    virtual bool fetchRemoteDirectoryContents(
        std::shared_ptr<const FileAttr> attr) const
    {
        return true;
    }

private:
    FsLogic &m_fsLogic;
    folly::fbstring m_storageId;
    std::map<folly::fbstring, std::shared_ptr<FileHandle>> m_fileHandles;
};

/**
 * @brief Virtual fs HelpersCache
 *
 * This class provides an implementation of a helpers cache for virtual fs
 * adapters. A new virtual file system adapter has to be registered using a
 * unique name which will be also used as the 'storageId'.
 */
class VirtualFsHelpersCache
    : public HelpersCacheBase,
      std::enable_shared_from_this<VirtualFsHelpersCache> {
public:
    /**
     * @brief Construct a new VirtualFs Helpers Cache object
     *
     * @param fsLogic Reference to an FsLogic instance
     */
    VirtualFsHelpersCache(FsLogic &fsLogic)
        : m_fsLogic{fsLogic}
    {
    }

    virtual ~VirtualFsHelpersCache() = default;

    /**
     * @brief Retrieves a helper instance.
     * @param fileUuid UUID of a file for which helper will be used.
     * @param spaceId SpaceId in the context of which the helper should be
     *                determined.
     * @param storageId Storage id for which to retrieve a helper.
     * @param forceProxyIO Determines whether to return a ProxyIO helper.
     * @return Retrieved future to helper instance shared pointer.
     */
    virtual folly::Future<HelpersCacheBase::HelperPtr> get(
        const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
        const folly::fbstring &storageId, bool forceProxyIO) override
    {
        return m_helpers.at(storageId);
    }

    /**
     * @brief Get the Access Type for file
     * Returns the storage access type for specific storage, if not
     * determined yet UNKNOWN value will be returned.
     *
     * @param storageId
     * @return HelpersCacheBase::AccessType
     */
    virtual HelpersCacheBase::AccessType getAccessType(
        const folly::fbstring &storageId) override
    {
        return HelpersCacheBase::AccessType::DIRECT;
    }

    /**
     * @brief Refresh helper parameters
     *
     * @param storageId
     * @param spaceId
     * @return folly::Future<folly::Unit>
     */
    folly::Future<folly::Unit> refreshHelperParameters(
        const folly::fbstring &storageId,
        const folly::fbstring &spaceId) override
    {
        return folly::makeFuture();
    }

    void add(const folly::fbstring &storageId,
        std::shared_ptr<VirtualFsAdapter> &&helper)
    {
        m_helpers.emplace(storageId, std::move(helper));
    }

    void readdir(const folly::fbstring &name,
        const folly::fbvector<folly::fbstring> &entries,
        std::shared_ptr<FileAttr> attr, MetadataCache &metadataCache)
    {
        for (auto &kv : m_helpers)
            if (kv.second->match(name))
                kv.second->readdir(entries, attr, metadataCache);
    }

    folly::fbstring match(const folly::fbstring &direntry)
    {
        for (const auto &kv : m_helpers)
            if (kv.second->match(direntry))
                return kv.first;

        return {};
    }

    std::shared_ptr<VirtualFsAdapter> get(const folly::fbstring &name)
    {
        return m_helpers.at(name);
    }

private:
    FsLogic &m_fsLogic;
    std::map<folly::fbstring, std::shared_ptr<VirtualFsAdapter>> m_helpers;
};
}
}
}

#endif
