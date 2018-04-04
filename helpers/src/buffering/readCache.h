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
#include "monitoring/monitoring.h"
#include "scheduler.h"

#include <boost/icl/discrete_interval.hpp>
#include <folly/CallOnce.h>
#include <folly/FBString.h>
#include <folly/fibers/TimedMutex.h>
#include <folly/futures/Future.h>
#include <folly/futures/SharedPromise.h>
#include <folly/io/IOBufQueue.h>

#include <atomic>
#include <chrono>
#include <cstdint>
#include <deque>

namespace one {
namespace helpers {
namespace buffering {

class ReadCache : public std::enable_shared_from_this<ReadCache> {
    using FiberMutex = folly::fibers::TimedMutex;

    struct ReadData {
        ReadData(
            const off_t offset_, const std::size_t size_, const bool isPrefetch)
            : offset{offset_}
            , size{size_}
        {
            if (!isPrefetch)
                folly::call_once(measureLatencyFlag, [] {});
        }

        std::chrono::steady_clock::time_point t() const
        {
            return std::chrono::steady_clock::time_point{t_.load()};
        }

        off_t offset;
        std::atomic<std::size_t> size;
        std::atomic<std::chrono::steady_clock::duration> t_{
            std::chrono::steady_clock::duration::max()};
        folly::once_flag measureLatencyFlag;
        folly::IOBufQueue buf;
        folly::SharedPromise<folly::Unit> promise;
    };

public:
    ReadCache(std::size_t readBufferMinSize, std::size_t readBufferMaxSize,
        std::chrono::seconds readBufferPrefetchDuration,
        double prefetchPowerBase, std::chrono::nanoseconds targetLatency,
        FileHandle &handle)
        : m_readBufferMinSize{readBufferMinSize}
        , m_readBufferMaxSize{readBufferMaxSize}
        , m_cacheDuration{readBufferPrefetchDuration * 2}
        , m_prefetchPowerBase{prefetchPowerBase}
        , m_targetLatency{static_cast<std::size_t>(targetLatency.count())}
        , m_handle{handle}
    {
        LOG_FCALL() << LOG_FARG(readBufferMinSize)
                    << LOG_FARG(readBufferMaxSize)
                    << LOG_FARG(readBufferPrefetchDuration.count());
    }

    folly::Future<folly::IOBufQueue> read(const off_t offset,
        const std::size_t size, const std::size_t continuousSize)
    {
        LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size);
        DCHECK(continuousSize >= size);

        std::unique_lock<FiberMutex> lock{m_mutex};
        if (m_clear) {
            LOG_DBG(2) << "Clearing cache for file " << m_handle.fileId();
            m_cache.clear();
            m_clear = false;
        }

        clearStaleEntries();

        if (isCurrentRead(offset)) {
            ONE_METRIC_COUNTER_ADD(
                "comp.helpers.mod.readcache.cacheHitBytes", size);
            ONE_METRIC_COUNTER_ADD(
                "comp.helpers.mod.readcache.cacheHitCount", 1);
            return readFromCache(offset, size);
        }

        ONE_METRIC_COUNTER_ADD(
            "comp.helpers.mod.readcache.cacheMissBytes", size);
        ONE_METRIC_COUNTER_ADD("comp.helpers.mod.readcache.cacheMissCount", 1);

        std::tie(m_blockSize, m_prefetchCoeff) =
            isSequential(offset) ? increaseBlockSize() : resetBlockSize();

        while (!m_cache.empty() && !isCurrentRead(offset))
            m_cache.pop_front();

        if (m_cache.empty())
            fetch(offset, size);

        prefetchIfNeeded(offset, continuousSize);

        return readFromCache(offset, size);
    }

    std::size_t wouldPrefetch(const off_t offset, const std::size_t /*size*/)
    {
        std::size_t prefetchSize = 0;
        if (!isCurrentRead(offset) && m_cache.size() < 2) {
            std::tie(prefetchSize, std::ignore) =
                isSequential(offset) ? increaseBlockSize() : resetBlockSize();
        }
        return prefetchSize;
    }

    void clear()
    {
        LOG_FCALL();

        std::unique_lock<FiberMutex> lock{m_mutex};
        m_clear = true;
    }

private:
    void clearStaleEntries()
    {
        auto now = std::chrono::steady_clock::now();
        while (!m_cache.empty() && m_cache.front()->t() < now - m_cacheDuration)
            m_cache.pop_front();
    }

    bool isSequential(const off_t offset)
    {
        if (m_cache.empty())
            return false; // no reference point

        if (m_cache.size() == 1)
            return offset ==
                static_cast<off_t>(
                    m_cache.front()->offset + m_cache.front()->size);

        return offset >= m_cache.back()->offset &&
            offset <
            static_cast<off_t>(m_cache.back()->offset + m_cache.back()->size);
    }

    void prefetchIfNeeded(const off_t offset, const std::size_t continuousSize)
    {
        LOG_FCALL();

        assert(!m_cache.empty());

        const off_t nextOffset = m_cache.back()->offset + m_cache.back()->size;
        const auto prefetchBlock =
            boost::icl::discrete_interval<off_t>::right_open(
                nextOffset, nextOffset + m_blockSize) &
            boost::icl::discrete_interval<off_t>::right_open(
                offset, offset + continuousSize);

        if (m_cache.size() < 2 && boost::icl::size(prefetchBlock) > 0) {
            LOG_DBG(2) << "Prefetching " << boost::icl::size(prefetchBlock)
                       << " bytes for file " << m_handle.fileId()
                       << " at offset " << nextOffset;

            prefetch(nextOffset, boost::icl::size(prefetchBlock));
        }
    }

    void prefetch(const off_t offset, const std::size_t size)
    {
        LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size);

        fetch(offset, size, true);
    }

    void fetch(const off_t offset, const std::size_t size)
    {
        LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size);

        fetch(offset, size, false);
    }

    void fetch(const off_t offset, const std::size_t size, bool isPrefetch)
    {
        LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size)
                    << LOG_FARG(isPrefetch);

        m_cache.emplace_back(
            std::make_shared<ReadData>(offset, size, isPrefetch));
        m_handle.read(offset, size)
            .then([readData = m_cache.back()](folly::IOBufQueue buf) {
                readData->size = buf.chainLength();
                readData->buf = std::move(buf);
                readData->t_ =
                    std::chrono::steady_clock::now().time_since_epoch();
                readData->promise.setValue();
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

#if !defined(NDEBUG)
        ONE_METRIC_COUNTER_SET("comp.helpers.mod.readcache." +
                m_handle.fileId().toStdString() + ".blockSize",
            m_blockSize);

        ONE_METRIC_COUNTER_SET("comp.helpers.mod.readcache." +
                m_handle.fileId().toStdString() + ".latency",
            m_latency);

        ONE_METRIC_COUNTER_SET("comp.helpers.mod.readcache." +
                m_handle.fileId().toStdString() + ".targetlatency",
            m_targetLatency);

        ONE_METRIC_COUNTER_SET("comp.helpers.mod.readcache." +
                m_handle.fileId().toStdString() + ".cachesize",
            std::accumulate(m_cache.begin(), m_cache.end(), 0,
                [](int acc, std::shared_ptr<ReadData> rd) {
                    return acc + rd->size;
                }));
#endif

        auto readData = m_cache.front();
        const auto startPoint = std::chrono::steady_clock::now();
        return readData->promise.getFuture().then([ =, s = weak_from_this() ] {
            folly::call_once(readData->measureLatencyFlag, [&] {
                if (auto self = s.lock()) {
                    const auto latency =
                        std::chrono::duration_cast<std::chrono::nanoseconds>(
                            std::chrono::steady_clock::now() - startPoint)
                            .count();

                    LOG_DBG(2)
                        << "Latest measured read latency for "
                        << m_handle.fileId() << " is " << m_latency << " ns";

                    m_latency = (m_latency + 2 * latency) / 3;

                    LOG_DBG(2)
                        << "Adjusted average read latency for "
                        << m_handle.fileId() << " to " << m_latency << " ns";
                }
            });

            folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
            if (readData->buf.empty() ||
                static_cast<off_t>(
                    readData->offset + readData->buf.chainLength()) < offset) {
                LOG_DBG(2) << "Latest block in read cache is empty or outside "
                              "requested range for file "
                           << m_handle.fileId();
                return buf;
            }

            buf.append(readData->buf.front()->clone());
            if (offset > readData->offset) {
                LOG_DBG(2) << "Trimming latest read cache block for file "
                           << m_handle.fileId()
                           << " to start at requested offset by: "
                           << offset - readData->offset;
                buf.trimStart(offset - readData->offset);
            }
            if (buf.chainLength() > size) {
                LOG_DBG(2) << "Trimming latest read cache block for file "
                           << m_handle.fileId()
                           << " to end at requested size by: "
                           << buf.chainLength() - size;
                buf.trimEnd(buf.chainLength() - size);
            }

            return buf;
        });
    }

    bool isCurrentRead(const off_t offset)
    {
        return !m_cache.empty() && m_cache.front()->offset <= offset &&
            offset <
            static_cast<off_t>(m_cache.front()->offset + m_cache.front()->size);
    }

    std::pair<std::size_t, double> resetBlockSize()
    {
        LOG_FCALL();
        return {0, 1.0};
    }

    std::pair<std::size_t, double> increaseBlockSize()
    {
        LOG_FCALL();

        auto blockSize = m_blockSize;
        auto prefetchCoeff = m_prefetchCoeff;
        if (m_blockSize < m_readBufferMaxSize &&
            m_latency.load() > m_targetLatency) {
            blockSize = std::min(m_readBufferMaxSize,
                m_readBufferMinSize *
                    static_cast<std::size_t>(m_prefetchCoeff));
            prefetchCoeff *= m_prefetchPowerBase;

            LOG_DBG(2) << "Adjusted prefetch block size for file "
                       << m_handle.fileId() << " to: " << blockSize
                       << " and prefetch coefficient to: " << prefetchCoeff;
        }

        return {blockSize, prefetchCoeff};
    }

    std::weak_ptr<ReadCache> weak_from_this() { return {shared_from_this()}; }

    const std::size_t m_readBufferMinSize;
    const std::size_t m_readBufferMaxSize;
    const std::chrono::seconds m_cacheDuration;
    const double m_prefetchPowerBase;
    const std::size_t m_targetLatency;
    FileHandle &m_handle;

    double m_prefetchCoeff = 1.0;
    std::size_t m_blockSize = 0;
    std::atomic<std::size_t> m_latency{m_targetLatency * 2};

    FiberMutex m_mutex;
    std::deque<std::shared_ptr<ReadData>> m_cache;
    bool m_clear{false};
    std::chrono::steady_clock::time_point m_lastCacheRefresh{};
};

} // namespace proxyio
} // namespace helpers
} // namespace one

#endif // HELPERS_BUFFERING_READ_CACHE_H
