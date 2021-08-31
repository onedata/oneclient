/**
 * @file archivematica.h
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ARCHIVEMATICA_H
#define ARCHIVEMATICA_H

#include "cache/helpersCache.h"
#include "virtualFsRegistry.h"

namespace folly {
class dynamic;
}

namespace one {
namespace client {

namespace fslogic {
class FsLogic;
} // namespace fslogic

namespace virtualfs {

namespace archivematica {

using one::client::cache::HelpersCacheBase;
using one::client::fslogic::FsLogic;
using one::helpers::FileHandle;
using one::helpers::FileHandlePtr;
using one::helpers::Params;
using one::helpers::StorageHelper;
using one::helpers::Timeout;
using one::helpers::WriteCallback;
using one::messages::fuse::FileAttr;

const auto kArchivematicaTimeout = 120 * 1'000;

class ArchivematicaVirtualFsAdapter : public VirtualFsAdapter {
public:
    ArchivematicaVirtualFsAdapter(
        FsLogic &fslogic, const folly::fbstring &storageId)
        : VirtualFsAdapter{fslogic, storageId}
    {
    }

    virtual folly::fbstring name() const;

    virtual bool match(const folly::fbstring &direntry) override;

    virtual folly::fbstring effectiveName(
        const folly::fbstring &direntry) override;

    virtual folly::Future<FileHandlePtr> open(const folly::fbstring &fileId,
        const int flags, const Params &openParams) override;

    virtual void readdir(const folly::fbvector<folly::fbstring> &entries,
        std::shared_ptr<const FileAttr> attr,
        MetadataCache &metadataCache) override;

    virtual bool fetchRemoteDirectoryContents(
        std::shared_ptr<const FileAttr> attr) const override;
};

class ArchivematicaProcessingMCPFileHandle : public FileHandle {
public:
    ArchivematicaProcessingMCPFileHandle(
        folly::fbstring fileId, std::shared_ptr<StorageHelper> helper);

    virtual ~ArchivematicaProcessingMCPFileHandle() = default;

    void updateContent(std::shared_ptr<const FileAttr> attr);

    std::unique_ptr<FileLocation> getLocation() const;

    virtual folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size) override;

    virtual folly::Future<std::size_t> write(const off_t offset,
        folly::IOBufQueue buf, WriteCallback &&writeCb) override;

    virtual const Timeout &timeout() override;

    size_t size() const { return m_content.size(); }

private:
    folly::dynamic getProcessingMCPMetadata(const folly::fbstring &uuid);

    folly::fbstring m_content;
    Timeout m_timeout{kArchivematicaTimeout};
};

class ArchivematicaMetadataJSONFileHandle : public FileHandle {
public:
    ArchivematicaMetadataJSONFileHandle(
        folly::fbstring fileId, std::shared_ptr<StorageHelper> helper);

    virtual ~ArchivematicaMetadataJSONFileHandle() = default;

    void updateContent(std::shared_ptr<const FileAttr> attr);

    std::unique_ptr<FileLocation> getLocation() const;

    virtual folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size) override;

    virtual folly::Future<std::size_t> write(const off_t offset,
        folly::IOBufQueue buf, WriteCallback &&writeCb) override;

    virtual const Timeout &timeout() override;

    size_t size() const { return m_content.size(); }

private:
    void appendMetadata(FsLogic &fsLogic, const folly::fbstring &parentUuid,
        folly::dynamic &objects, const folly::fbstring &prefix,
        bool topLevelDir = false);

    folly::fbstring m_content;
    Timeout m_timeout{kArchivematicaTimeout};
};
} // namespace archivematica
} // namespace virtualfs
} // namespace client
} // namespace one

#endif
