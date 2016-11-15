/**
 * @file bufferAgent.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_BUFFERING_BUFFER_AGENT_H
#define HELPERS_BUFFERING_BUFFER_AGENT_H

#include "readCache.h"
#include "writeBuffer.h"

#include "communication/communicator.h"
#include "helpers/storageHelper.h"
#include "scheduler.h"

#include <glog/logging.h>

#include <memory>
#include <mutex>

namespace one {
namespace helpers {
namespace buffering {

struct BufferLimits {
    std::size_t maxGlobalReadCacheSize = 1024 * 1024 * 1024;
    std::size_t maxGlobalWriteBufferSize = 1024 * 1024 * 1024;

    std::size_t minReadChunkSize = 1 * 1024 * 1024;
    std::size_t maxReadChunkSize = 50 * 1024 * 1024;
    std::chrono::seconds readAheadFor = std::chrono::seconds{1};

    std::size_t minWriteChunkSize = 1 * 1024 * 1024;
    std::size_t maxWriteChunkSize = 50 * 1024 * 1024;
    std::chrono::seconds flushWriteAfter = std::chrono::seconds{1};
};

class BufferedFileHandle : public FileHandle {
public:
    BufferedFileHandle(folly::fbstring fileId, FileHandlePtr wrappedHandle,
        const BufferLimits &bl, Scheduler &scheduler)
        : FileHandle{std::move(fileId)}
        , m_wrappedHandle{std::move(wrappedHandle)}
        , m_scheduler{scheduler}
        , m_readCache{std::make_shared<ReadCache>(bl.minReadChunkSize,
              bl.maxReadChunkSize, bl.readAheadFor, *m_wrappedHandle)}
        , m_writeBuffer{std::make_shared<WriteBuffer>(bl.minWriteChunkSize,
              bl.maxWriteChunkSize, bl.flushWriteAfter, *m_wrappedHandle,
              m_scheduler, m_readCache)}
    {
        m_writeBuffer->scheduleFlush();
    }

    folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size) override
    {
        // Push all changes so we'll always read data that we just wrote. A
        // mechanism in `WriteBuffer` will trigger a clear of the readCache if
        // needed. This might be optimized in the future by modifying readcache
        // on write.
        return m_writeBuffer->fsync().then(
            [this, offset, size] { return m_readCache->read(offset, size); });
    }

    folly::Future<std::size_t> write(
        const off_t offset, folly::IOBufQueue buf) override
    {
        return m_writeBuffer->write(offset, std::move(buf));
    }

    folly::Future<folly::Unit> fsync(bool isDataSync) override
    {
        return m_writeBuffer->fsync().then([
            readCache = m_readCache, wrappedHandle = m_wrappedHandle, isDataSync
        ] {
            readCache->clear();
            return wrappedHandle->fsync(isDataSync);
        });
    }

    folly::Future<folly::Unit> flush() override
    {
        return m_writeBuffer->fsync().then(
            [ readCache = m_readCache, wrappedHandle = m_wrappedHandle ] {
                readCache->clear();
                return wrappedHandle->flush();
            });
    }

    folly::Future<folly::Unit> release() override
    {
        return m_writeBuffer->fsync().then(
            [wrappedHandle = m_wrappedHandle] { wrappedHandle->release(); });
    }

    bool needsDataConsistencyCheck() override
    {
        return m_wrappedHandle->needsDataConsistencyCheck();
    }

private:
    FileHandlePtr m_wrappedHandle;
    Scheduler &m_scheduler;
    std::shared_ptr<ReadCache> m_readCache;
    std::shared_ptr<WriteBuffer> m_writeBuffer;
};

class BufferAgent : public StorageHelper {
public:
    BufferAgent(BufferLimits bufferLimits, StorageHelperPtr helper,
        Scheduler &scheduler)
        : m_bufferLimits{bufferLimits}
        , m_helper{std::move(helper)}
        , m_scheduler{scheduler}
    {
    }

    folly::Future<FileHandlePtr> open(const folly::fbstring &fileId,
        const int flags, const Params &params) override
    {
        return m_helper->open(fileId, flags, params).then([
            fileId, bl = m_bufferLimits, &scheduler = m_scheduler
        ](FileHandlePtr handle) {
            return std::make_shared<BufferedFileHandle>(
                std::move(fileId), std::move(handle), std::move(bl), scheduler);
        });
    }

    folly::Future<struct stat> getattr(const folly::fbstring &fileId) override
    {
        return m_helper->getattr(fileId);
    }

    folly::Future<folly::Unit> access(
        const folly::fbstring &fileId, const int mask) override
    {
        return m_helper->access(fileId, mask);
    }

    folly::Future<folly::fbstring> readlink(
        const folly::fbstring &fileId) override
    {
        return m_helper->readlink(fileId);
    }

    folly::Future<folly::fbvector<folly::fbstring>> readdir(
        const folly::fbstring &fileId, const off_t offset,
        const std::size_t count)
    {
        return m_helper->readdir(fileId, offset, count);
    }

    folly::Future<folly::Unit> mknod(const folly::fbstring &fileId,
        const mode_t mode, const FlagsSet &flags, const dev_t rdev)
    {
        return m_helper->mknod(fileId, mode, flags, rdev);
    }

    folly::Future<folly::Unit> mkdir(
        const folly::fbstring &fileId, const mode_t mode)
    {
        return m_helper->mkdir(fileId, mode);
    }

    folly::Future<folly::Unit> unlink(const folly::fbstring &fileId)
    {
        return m_helper->unlink(fileId);
    }

    folly::Future<folly::Unit> rmdir(const folly::fbstring &fileId)
    {
        return m_helper->rmdir(fileId);
    }

    folly::Future<folly::Unit> symlink(
        const folly::fbstring &from, const folly::fbstring &to)
    {
        return m_helper->symlink(from, to);
    }

    folly::Future<folly::Unit> rename(
        const folly::fbstring &from, const folly::fbstring &to)
    {
        return m_helper->rename(from, to);
    }

    folly::Future<folly::Unit> link(
        const folly::fbstring &from, const folly::fbstring &to)
    {
        return m_helper->link(from, to);
    }

    folly::Future<folly::Unit> chmod(
        const folly::fbstring &fileId, const mode_t mode)
    {
        return m_helper->chmod(fileId, mode);
    }

    folly::Future<folly::Unit> chown(
        const folly::fbstring &fileId, const uid_t uid, const gid_t gid)
    {
        return m_helper->chown(fileId, uid, gid);
    }

    folly::Future<folly::Unit> truncate(
        const folly::fbstring &fileId, const off_t size)
    {
        return m_helper->truncate(fileId, size);
    }

private:
    BufferLimits m_bufferLimits;
    StorageHelperPtr m_helper;
    Scheduler &m_scheduler;
};

} // namespace proxyio
} // namespace helpers
} // namespace one

#endif // HELPERS_BUFFERING_BUFFER_AGENT_H
