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
#include "helpers/IStorageHelper.h"
#include "scheduler.h"

#include <glog/logging.h>

#include <memory>
#include <mutex>

namespace one {
namespace helpers {
namespace buffering {

class BufferAgentCTX : public IStorageHelperCTX {
public:
    BufferAgentCTX(std::unordered_map<std::string, std::string> params)
        : IStorageHelperCTX{std::move(params)}
    {
    }

    void setUserCTX(std::unordered_map<std::string, std::string> args) override
    {
        helperCtx->setUserCTX(std::move(args));
    }

    std::unordered_map<std::string, std::string> getUserCTX() override
    {
        return helperCtx->getUserCTX();
    }

    CTXPtr helperCtx;
    std::shared_ptr<ReadCache> readCache;
    std::shared_ptr<WriteBuffer> writeBuffer;
    std::mutex mutex;
};

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

class BufferAgent : public IStorageHelper {
public:
    BufferAgent(BufferLimits bufferLimits,
        std::unique_ptr<IStorageHelper> helper, Scheduler &scheduler)
        : m_bufferLimits{bufferLimits}
        , m_helper{std::move(helper)}
        , m_scheduler{scheduler}
    {
    }

    CTXPtr createCTX(
        std::unordered_map<std::string, std::string> params) override
    {
        auto ctx = std::make_shared<BufferAgentCTX>(params);
        ctx->helperCtx = m_helper->createCTX(std::move(params));
        return ctx;
    }

    int sh_open(
        CTXPtr rawCtx, const boost::filesystem::path &p, int flags) override
    {
        auto ctx = getCTX(rawCtx);
        std::lock_guard<std::mutex> guard{ctx->mutex};

        const auto &bl = m_bufferLimits;

        ctx->readCache = std::make_shared<ReadCache>(bl.minReadChunkSize,
            bl.maxReadChunkSize, bl.readAheadFor, *m_helper);

        ctx->writeBuffer = std::make_shared<WriteBuffer>(bl.minWriteChunkSize,
            bl.maxWriteChunkSize, bl.flushWriteAfter, *m_helper, m_scheduler,
            ctx->readCache);
        ctx->writeBuffer->scheduleFlush();

        return m_helper->sh_open(ctx->helperCtx, p, flags);
    }

    asio::mutable_buffer sh_read(CTXPtr rawCtx,
        const boost::filesystem::path &p, asio::mutable_buffer buf,
        off_t offset) override
    {
        auto ctx = getCTX(rawCtx);
        std::lock_guard<std::mutex> guard{ctx->mutex};

        if (!ctx->writeBuffer)
            return m_helper->sh_read(ctx->helperCtx, p, buf, offset);

        // Push all changes so we'll always read data that we just wrote. A
        // mechanism in `WriteBuffer` will trigger a clear of the readCache if
        // needed. This might be optimized in the future by modifying readcache
        // on write.
        ctx->writeBuffer->fsync(ctx->helperCtx, p);

        return ctx->readCache->read(ctx->helperCtx, p, buf, offset);
    }

    std::size_t sh_write(CTXPtr rawCtx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset) override
    {
        auto ctx = getCTX(rawCtx);
        std::lock_guard<std::mutex> guard{ctx->mutex};

        if (!ctx->readCache)
            return m_helper->sh_write(ctx->helperCtx, p, buf, offset);

        return ctx->writeBuffer->write(ctx->helperCtx, p, buf, offset);
    }

    void sh_flush(CTXPtr rawCtx, const boost::filesystem::path &p) override
    {
        auto ctx = getCTX(rawCtx);
        std::lock_guard<std::mutex> guard{ctx->mutex};

        if (ctx->writeBuffer && ctx->readCache) {
            ctx->writeBuffer->flush(ctx->helperCtx, p);
            ctx->readCache->clear();
        }

        m_helper->sh_flush(ctx->helperCtx, p);
    }

    void sh_fsync(CTXPtr rawCtx, const boost::filesystem::path &p,
        bool isDataSync) override
    {
        auto ctx = getCTX(rawCtx);
        std::lock_guard<std::mutex> guard{ctx->mutex};

        if (ctx->writeBuffer && ctx->readCache) {
            ctx->writeBuffer->fsync(ctx->helperCtx, p);
            ctx->readCache->clear();
        }

        m_helper->sh_fsync(ctx->helperCtx, p, isDataSync);
    }

    void sh_release(CTXPtr rawCtx, const boost::filesystem::path &p) override
    {
        auto ctx = getCTX(rawCtx);
        std::lock_guard<std::mutex> guard{ctx->mutex};

        if (ctx->writeBuffer)
            ctx->writeBuffer->fsync(ctx->helperCtx, p);

        m_helper->sh_release(ctx->helperCtx, p);
    }

    void ash_getattr(CTXPtr rawCtx, const boost::filesystem::path &p,
        GeneralCallback<struct stat> callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_getattr(ctx->helperCtx, p, std::move(callback));
    }

    void ash_access(CTXPtr rawCtx, const boost::filesystem::path &p, int mask,
        VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_access(ctx->helperCtx, p, mask, std::move(callback));
    }

    void ash_readlink(CTXPtr rawCtx, const boost::filesystem::path &p,
        GeneralCallback<std::string> callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_readlink(ctx->helperCtx, p, std::move(callback));
    }

    void ash_readdir(CTXPtr rawCtx, const boost::filesystem::path &p,
        off_t offset, size_t count,
        GeneralCallback<const std::vector<std::string> &> callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_readdir(
            ctx->helperCtx, p, offset, count, std::move(callback));
    }

    void ash_mknod(CTXPtr rawCtx, const boost::filesystem::path &p, mode_t mode,
        FlagsSet flags, dev_t rdev, VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_mknod(ctx->helperCtx, p, mode, std::move(flags), rdev,
            std::move(callback));
    }

    void ash_mkdir(CTXPtr rawCtx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_mkdir(ctx->helperCtx, p, mode, std::move(callback));
    }

    void ash_unlink(CTXPtr rawCtx, const boost::filesystem::path &p,
        VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_unlink(ctx->helperCtx, p, std::move(callback));
    }

    void ash_rmdir(CTXPtr rawCtx, const boost::filesystem::path &p,
        VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_rmdir(ctx->helperCtx, p, std::move(callback));
    }

    void ash_symlink(CTXPtr rawCtx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_symlink(ctx->helperCtx, from, to, std::move(callback));
    }

    void ash_rename(CTXPtr rawCtx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_rename(ctx->helperCtx, from, to, std::move(callback));
    }

    void ash_link(CTXPtr rawCtx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_link(ctx->helperCtx, from, to, std::move(callback));
    }

    void ash_chmod(CTXPtr rawCtx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_chmod(ctx->helperCtx, p, mode, std::move(callback));
    }

    void ash_chown(CTXPtr rawCtx, const boost::filesystem::path &p, uid_t uid,
        gid_t gid, VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_chown(ctx->helperCtx, p, uid, gid, std::move(callback));
    }

    void ash_truncate(CTXPtr rawCtx, const boost::filesystem::path &p,
        off_t size, VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_truncate(ctx->helperCtx, p, size, std::move(callback));
    }

    void ash_open(CTXPtr rawCtx, const boost::filesystem::path &p,
        FlagsSet flags, GeneralCallback<int> callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_open(
            ctx->helperCtx, p, std::move(flags), std::move(callback));
    }

    void ash_open(CTXPtr rawCtx, const boost::filesystem::path &p, int flags,
        GeneralCallback<int> callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_open(ctx->helperCtx, p, flags, std::move(callback));
    }

    void ash_read(CTXPtr rawCtx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        GeneralCallback<asio::mutable_buffer> callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_read(ctx->helperCtx, p, buf, offset, std::move(callback));
    }

    void ash_write(CTXPtr rawCtx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset,
        GeneralCallback<std::size_t> callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_write(
            ctx->helperCtx, p, buf, offset, std::move(callback));
    }

    void ash_multiwrite(CTXPtr rawCtx, const boost::filesystem::path &p,
        std::vector<std::pair<off_t, asio::const_buffer>> buffs,
        GeneralCallback<std::size_t> callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_multiwrite(
            ctx->helperCtx, p, std::move(buffs), std::move(callback));
    }

    void ash_release(CTXPtr rawCtx, const boost::filesystem::path &p,
        VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_release(ctx->helperCtx, p, std::move(callback));
    }

    void ash_flush(CTXPtr rawCtx, const boost::filesystem::path &p,
        VoidCallback callback) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_flush(ctx->helperCtx, p, std::move(callback));
    }

    void ash_fsync(CTXPtr rawCtx, const boost::filesystem::path &p,
        bool isDataSync, VoidCallback callback)
    {
        auto ctx = getCTX(rawCtx);
        m_helper->ash_fsync(ctx->helperCtx, p, isDataSync, std::move(callback));
    }

    void sh_truncate(
        CTXPtr rawCtx, const boost::filesystem::path &p, off_t size) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->sh_truncate(ctx->helperCtx, p, size);
    }

    void sh_unlink(CTXPtr rawCtx, const boost::filesystem::path &p) override
    {
        auto ctx = getCTX(rawCtx);
        m_helper->sh_unlink(ctx->helperCtx, p);
    }

    bool needsDataConsistencyCheck() override
    {
        return m_helper->needsDataConsistencyCheck();
    }

private:
    std::shared_ptr<BufferAgentCTX> getCTX(const CTXPtr &rawCTX)
    {
        auto ctx = std::dynamic_pointer_cast<BufferAgentCTX>(rawCTX);
        if (ctx == nullptr) {
            // TODO: This doesn't really make sense; VFS-1956 is the only hope.
            // Anyway, the current plan is to have an empty context so that
            // there are no buffers (because of current context design buffers
            // the buffers would be recreated every call anyway).
            LOG(INFO) << "Helper changed. Creating new unbuffered BufferAgent "
                         "context.";

            return std::static_pointer_cast<BufferAgentCTX>(
                createCTX(rawCTX->parameters()));
        }
        return ctx;
    }

    BufferLimits m_bufferLimits;
    std::unique_ptr<IStorageHelper> m_helper;
    Scheduler &m_scheduler;
};

} // namespace proxyio
} // namespace helpers
} // namespace one

#endif // HELPERS_BUFFERING_BUFFER_AGENT_H
