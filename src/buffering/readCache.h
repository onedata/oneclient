/**
 * @file readCache.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_BUFFERING_READ_CACHE_H
#define HELPERS_BUFFERING_READ_CACHE_H

#include "communication/communicator.h"
#include "helpers/storageHelper.h"
#include "logging.h"
#include "messages/proxyio/remoteData.h"
#include "messages/proxyio/remoteRead.h"
#include "scheduler.h"

#include <folly/FBString.h>
#include <folly/fibers/TimedMutex.h>
#include <folly/futures/Future.h>
#include <folly/futures/SharedPromise.h>
#include <folly/io/IOBufQueue.h>

#include <atomic>
#include <chrono>
#include <cstdint>
#include <queue>

namespace one {
namespace helpers {
namespace buffering {

class ReadCache : public std::enable_shared_from_this<ReadCache> {
    using FiberMutex = folly::fibers::TimedMutex;

    struct ReadData {
        ReadData(const off_t offset_, const std::size_t size_)
            : offset{offset_}
            , size{size_}
        {
        }

        off_t offset;
        std::atomic<std::size_t> size;
        folly::SharedPromise<std::shared_ptr<folly::IOBufQueue>> promise;
    };

public:
    ReadCache(std::size_t readBufferMinSize, std::size_t readBufferMaxSize,
        std::chrono::seconds readBufferPrefetchDuration, FileHandle &handle)
        : m_readBufferMinSize{readBufferMinSize}
        , m_readBufferMaxSize{readBufferMaxSize}
        , m_readBufferPrefetchDuration{readBufferPrefetchDuration}
        , m_cacheDuration{readBufferPrefetchDuration * 2}
        , m_handle{handle}
    {
        LOG_FCALL() << LOG_FARG(readBufferMinSize)
                    << LOG_FARG(readBufferMaxSize)
                    << LOG_FARG(readBufferPrefetchDuration.count());
    }

    folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size)
    {
        LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size);

        std::unique_lock<FiberMutex> lock{m_mutex};
        if (isStale()) {
            while (!m_cache.empty()) {
                m_cache.pop();
            }
            m_clear = false;
        }

        if (isCurrentRead(offset))
            return readFromCache(offset, size);

        m_lastCacheRefresh = std::chrono::steady_clock::now();

        while (!m_cache.empty() && !isCurrentRead(offset))
            m_cache.pop();

        if (m_cache.empty())
            fetch(offset, size);

        prefetchIfNeeded();

        return readFromCache(offset, size);
    }

    void clear()
    {
        LOG_FCALL();

        std::unique_lock<FiberMutex> lock{m_mutex};
        m_clear = true;
    }

private:
    void prefetchIfNeeded()
    {
        LOG_FCALL();

        assert(!m_cache.empty());

        while (m_cache.size() < 2) {
            const auto nextOffset =
                m_cache.back()->offset + m_cache.back()->size;
            fetch(nextOffset, blockSize());
        }
    }

    void fetch(const off_t offset, const std::size_t size)
    {
        LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size);

        const auto startPoint = std::chrono::steady_clock::now();

        m_cache.emplace(std::make_shared<ReadData>(offset, size));
        m_handle.read(offset, size)
            .then([
                startPoint, readData = m_cache.back(),
                s = std::weak_ptr<ReadCache>{shared_from_this()}
            ](folly::IOBufQueue && buf) {
                if (auto self = s.lock()) {
                    const auto duration =
                        std::chrono::duration_cast<std::chrono::nanoseconds>(
                            std::chrono::steady_clock::now() - startPoint)
                            .count();

                    if (duration > 0) {
                        auto bandwidth =
                            buf.chainLength() * 1000000000 / duration;

                        self->m_bps = (self->m_bps * 1 + bandwidth * 2) / 3;
                    }
                }

                readData->size = buf.chainLength();
                readData->promise.setValue(
                    std::make_shared<folly::IOBufQueue>(std::move(buf)));
            })
            .onError([readData = m_cache.back()](folly::exception_wrapper ew) {
                readData->promise.setException(std::move(ew));
            });
    }

    folly::Future<folly::IOBufQueue> readFromCache(
        const off_t offset, const std::size_t size)
    {
        LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size);

        assert(!m_cache.empty());
        return m_cache.front()->promise.getFuture().then([
            offset, size, cachedOffset = m_cache.front()->offset
        ](std::shared_ptr<folly::IOBufQueue> cachedBuf) {

            folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
            if (cachedBuf->empty() ||
                static_cast<off_t>(cachedOffset + cachedBuf->chainLength()) <
                    offset)
                return buf;

            buf.append(cachedBuf->front()->clone());
            if (offset > cachedOffset)
                buf.trimStart(offset - cachedOffset);
            if (buf.chainLength() > size)
                buf.trimEnd(buf.chainLength() - size);

            return buf;
        });
    }

    bool isCurrentRead(const off_t offset)
    {
        return !m_cache.empty() && m_cache.front()->offset <= offset &&
            offset <
            static_cast<off_t>(m_cache.front()->offset + m_cache.front()->size);
    }

    bool isStale() const
    {
        return m_clear ||
            m_lastCacheRefresh + m_cacheDuration <
            std::chrono::steady_clock::now();
    }

    std::size_t blockSize()
    {
        return std::min(m_readBufferMaxSize,
                   std::max(m_readBufferMinSize, m_bps.load())) *
            m_readBufferPrefetchDuration.count();
    }

    const std::size_t m_readBufferMinSize;
    const std::size_t m_readBufferMaxSize;
    const std::chrono::seconds m_readBufferPrefetchDuration;
    const std::chrono::seconds m_cacheDuration;
    FileHandle &m_handle;

    std::atomic<std::size_t> m_bps{0};

    FiberMutex m_mutex;
    std::queue<std::shared_ptr<ReadData>> m_cache;
    bool m_clear{false};
    std::chrono::steady_clock::time_point m_lastCacheRefresh{};
};

} // namespace proxyio
} // namespace helpers
} // namespace one

#endif // HELPERS_BUFFERING_READ_CACHE_H
