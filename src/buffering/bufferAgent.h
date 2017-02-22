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
#include "helpers/storageHelperCreator.h"
#include "scheduler.h"

#include <glog/logging.h>

#include <chrono>
#include <memory>
#include <mutex>

namespace one {
namespace helpers {
namespace buffering {

class BufferedFileHandle : public FileHandle {
public:
    BufferedFileHandle(folly::fbstring fileId, FileHandlePtr wrappedHandle,
        const BufferLimits &bl, Scheduler &scheduler)
        : FileHandle{std::move(fileId)}
        , m_wrappedHandle{std::move(wrappedHandle)}
        , m_scheduler{scheduler}
        , m_readCache{std::make_shared<ReadCache>(bl.readBufferMinSize,
              bl.readBufferMaxSize, bl.readBufferPrefetchDuration,
              *m_wrappedHandle)}
        , m_writeBuffer{std::make_shared<WriteBuffer>(bl.writeBufferMinSize,
              bl.writeBufferMaxSize, bl.writeBufferFlushDelay,
              *m_wrappedHandle, m_scheduler, m_readCache)}
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

    const Timeout &timeout() override { return m_wrappedHandle->timeout(); }

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
        : m_bufferLimits{std::move(bufferLimits)}
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
                std::move(fileId), std::move(handle), bl, scheduler);
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
        const std::size_t count) override
    {
        return m_helper->readdir(fileId, offset, count);
    }

    folly::Future<folly::Unit> mknod(const folly::fbstring &fileId,
        const mode_t mode, const FlagsSet &flags, const dev_t rdev) override
    {
        return m_helper->mknod(fileId, mode, flags, rdev);
    }

    folly::Future<folly::Unit> mkdir(
        const folly::fbstring &fileId, const mode_t mode) override
    {
        return m_helper->mkdir(fileId, mode);
    }

    folly::Future<folly::Unit> unlink(const folly::fbstring &fileId) override
    {
        return m_helper->unlink(fileId);
    }

    folly::Future<folly::Unit> rmdir(const folly::fbstring &fileId) override
    {
        return m_helper->rmdir(fileId);
    }

    folly::Future<folly::Unit> symlink(
        const folly::fbstring &from, const folly::fbstring &to) override
    {
        return m_helper->symlink(from, to);
    }

    folly::Future<folly::Unit> rename(
        const folly::fbstring &from, const folly::fbstring &to) override
    {
        return m_helper->rename(from, to);
    }

    folly::Future<folly::Unit> link(
        const folly::fbstring &from, const folly::fbstring &to) override
    {
        return m_helper->link(from, to);
    }

    folly::Future<folly::Unit> chmod(
        const folly::fbstring &fileId, const mode_t mode) override
    {
        return m_helper->chmod(fileId, mode);
    }

    folly::Future<folly::Unit> chown(
        const folly::fbstring &fileId, const uid_t uid, const gid_t gid) override
    {
        return m_helper->chown(fileId, uid, gid);
    }

    folly::Future<folly::Unit> truncate(
        const folly::fbstring &fileId, const off_t size) override
    {
        return m_helper->truncate(fileId, size);
    }

    const Timeout &timeout() override { return m_helper->timeout(); }

private:
    BufferLimits m_bufferLimits;
    StorageHelperPtr m_helper;
    Scheduler &m_scheduler;
};

} // namespace proxyio
} // namespace helpers
} // namespace one

#endif // HELPERS_BUFFERING_BUFFER_AGENT_H
