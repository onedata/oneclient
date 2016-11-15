/**
 * @file writeBuffer.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_BUFFERING_WRITE_BUFFER_H
#define HELPERS_BUFFERING_WRITE_BUFFER_H

#include "readCache.h"

#include "communication/communicator.h"
#include "helpers/storageHelper.h"
#include "messages/proxyio/remoteWrite.h"
#include "messages/proxyio/remoteWriteResult.h"
#include "scheduler.h"

#include <asio/buffer.hpp>
#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/fibers/Baton.h>
#include <folly/fibers/TimedMutex.h>
#include <folly/futures/Future.h>

#include <atomic>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <memory>
#include <mutex>
#include <queue>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

namespace one {
namespace helpers {
namespace buffering {

class WriteBuffer : public std::enable_shared_from_this<WriteBuffer> {
    using FiberMutex = folly::fibers::TimedMutex<folly::fibers::Baton>;

public:
    WriteBuffer(const std::size_t minWriteChunkSize,
        const std::size_t maxWriteChunkSize,
        const std::chrono::seconds flushWriteAfter, FileHandle &handle,
        Scheduler &scheduler, std::shared_ptr<ReadCache> readCache)
        : m_minWriteChunkSize{minWriteChunkSize}
        , m_maxWriteChunkSize{maxWriteChunkSize}
        , m_flushWriteAfter{flushWriteAfter}
        , m_handle{handle}
        , m_scheduler{scheduler}
        , m_readCache{readCache}
    {
    }

    ~WriteBuffer() { m_cancelFlushSchedule(); }

    folly::Future<std::size_t> write(const off_t offset, folly::IOBufQueue buf)
    {
        std::unique_lock<FiberMutex> lock{m_mutex};

        m_cancelFlushSchedule();
        scheduleFlush();

        const std::size_t size = buf.chainLength();

        if (m_buffers.empty() || m_nextOffset != offset)
            m_buffers.emplace_back(offset, std::move(buf));
        else
            m_buffers.back().second.append(std::move(buf));

        m_bufferedSize += size;
        m_nextOffset = offset + size;

        if (m_bufferedSize > calculateFlushThreshold()) {
            // We're always returning "everything" on success, so provider has
            // to try to save everything and return an error if not successful.
            pushBuffer();
            return confirmOverThreshold().then([size] { return size; });
        }

        return folly::makeFuture(size);
    }

    folly::Future<folly::Unit> fsync()
    {
        std::unique_lock<FiberMutex> lock{m_mutex};
        pushBuffer();
        return confirmAll();
    }

    void scheduleFlush()
    {
        m_cancelFlushSchedule = m_scheduler.schedule(m_flushWriteAfter,
            [s = std::weak_ptr<WriteBuffer>(shared_from_this())] {
                if (auto self = s.lock()) {
                    std::unique_lock<FiberMutex> lock{self->m_mutex};
                    self->pushBuffer();
                    self->scheduleFlush();
                }
            });
    }

private:
    void pushBuffer()
    {
        if (m_bufferedSize == 0)
            return;

        decltype(m_buffers) buffers;
        buffers.swap(m_buffers);

        auto sentSize = m_bufferedSize;
        m_pendingConfirmation += sentSize;
        m_bufferedSize = 0;

        const auto startPoint = std::chrono::steady_clock::now();

        auto writeFuture = m_handle.multiwrite(std::move(buffers)).then([
            startPoint, sentSize,
            s = std::weak_ptr<WriteBuffer>(shared_from_this())
        ](std::size_t) {
            auto self = s.lock();
            if (!self)
                return;

            auto duration =
                std::chrono::duration_cast<std::chrono::nanoseconds>(
                    std::chrono::steady_clock::now() - startPoint)
                    .count();

            if (duration > 0) {
                auto bandwidth = sentSize * 1000000000 / duration;
                self->m_bps = (self->m_bps * 1 + bandwidth * 2) / 3;
            }

            self->m_readCache->clear();
        });

        m_writeFutures.emplace(
            std::make_pair(sentSize, std::move(writeFuture)));
    }

    folly::Future<folly::Unit> confirmOverThreshold()
    {
        return confirm(calculateConfirmThreshold());
    }

    folly::Future<folly::Unit> confirmAll() { return confirm(0); }

    folly::Future<folly::Unit> confirm(const std::size_t threshold)
    {
        folly::fbvector<folly::Future<folly::Unit>> confirmFutures;

        while (m_pendingConfirmation > threshold) {
            confirmFutures.emplace_back(
                std::move(m_writeFutures.front().second));

            m_pendingConfirmation -= m_writeFutures.front().first;
            m_writeFutures.pop();
        }

        return folly::collectAll(confirmFutures)
            .then([](const std::vector<folly::Try<folly::Unit>> &tries) {
                for (const auto &t : tries) {
                    if (t.hasException())
                        return folly::makeFuture<folly::Unit>(t.exception());
                }

                return folly::makeFuture();
            });
    }

    std::size_t calculateFlushThreshold()
    {
        return std::min(
            m_maxWriteChunkSize, std::max(m_minWriteChunkSize, 2 * m_bps));
    }

    std::size_t calculateConfirmThreshold()
    {
        return 6 * calculateFlushThreshold();
    }

    std::size_t m_minWriteChunkSize;
    std::size_t m_maxWriteChunkSize;
    std::chrono::seconds m_flushWriteAfter;

    FileHandle &m_handle;
    Scheduler &m_scheduler;
    std::shared_ptr<ReadCache> m_readCache;

    std::function<void()> m_cancelFlushSchedule;

    std::size_t m_bufferedSize = 0;
    off_t m_nextOffset = 0;
    folly::fbvector<std::pair<off_t, folly::IOBufQueue>> m_buffers;
    std::atomic<std::size_t> m_bps{0};

    FiberMutex m_mutex;
    std::size_t m_pendingConfirmation = 0;
    std::queue<std::pair<off_t, folly::Future<folly::Unit>>> m_writeFutures;
};

} // namespace proxyio
} // namespace helpers
} // namespace one

#endif // HELPERS_BUFFERING_WRITE_BUFFER_H
