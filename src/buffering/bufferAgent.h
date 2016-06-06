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
