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
#include "helpers/IStorageHelper.h"
#include "messages/proxyio/remoteWrite.h"
#include "messages/proxyio/remoteWriteResult.h"
#include "scheduler.h"

#include <asio/buffer.hpp>

#include <chrono>
#include <cmath>
#include <cstdint>
#include <memory>
#include <mutex>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

namespace one {
namespace helpers {
namespace buffering {

/**
 * Writes to a single file are serialized by the kernel, so this class doesn't
 * have to be thread-safe. A mutex is introduced to synchronize between writes
 * and scheduled flush.
 */
class WriteBuffer : public std::enable_shared_from_this<WriteBuffer> {
    struct ByteSequence {
        ByteSequence(const off_t offset_, asio::const_buffer buf)
            : offset{offset_}
            , data{asio::buffer_cast<const char *>(buf), asio::buffer_size(buf)}
        {
        }

        off_t nextOffset() const { return offset + data.size(); }

        void append(asio::const_buffer buf) {}

        off_t offset;
        std::string data;
    };

public:
    WriteBuffer(const std::size_t minWriteChunkSize,
        const std::size_t maxWriteChunkSize,
        const std::chrono::seconds flushWriteAfter, IStorageHelper &helper,
        Scheduler &scheduler, std::shared_ptr<ReadCache> readCache)
        : m_minWriteChunkSize{minWriteChunkSize}
        , m_maxWriteChunkSize{maxWriteChunkSize}
        , m_flushWriteAfter{flushWriteAfter}
        , m_helper{helper}
        , m_scheduler{scheduler}
        , m_readCache{readCache}
    {
    }

    ~WriteBuffer() { m_cancelFlushSchedule(); }

    std::size_t write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, const off_t offset)
    {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_lastCtx = ctx;
        m_lastPath = p;

        throwLastError();

        m_cancelFlushSchedule();
        scheduleFlush();

        if (m_buffers.empty() || nextOffset() != offset)
            emplace(offset, buf);
        else
            append(buf);

        m_bufferedSize += asio::buffer_size(buf);

        if (m_bufferedSize > flushThreshold()) {
            // We're always returning "everything" on success, so provider has
            // to try to save everything and return an error if not successful.
            pushBuffer(ctx, p, lock);
        }

        return asio::buffer_size(buf);
    }

    void flush(CTXPtr ctx, const boost::filesystem::path &p)
    {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_lastCtx = ctx;
        m_lastPath = p;

        throwLastError();

        pushBuffer(ctx, p, lock);
    }

    void fsync(CTXPtr ctx, const boost::filesystem::path &p)
    {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_lastCtx = ctx;
        m_lastPath = p;

        throwLastError();

        pushBuffer(ctx, p, lock);

        while (!m_lastError && m_pendingConfirmation > 0)
            if (m_confirmationCondition.wait_for(
                    lock, std::chrono::minutes{2}) == std::cv_status::timeout)
                m_lastError = std::make_error_code(std::errc::timed_out);

        throwLastError();
    }

    void scheduleFlush()
    {
        m_cancelFlushSchedule = m_scheduler.schedule(m_flushWriteAfter,
            [s = std::weak_ptr<WriteBuffer>(shared_from_this())] {
                if (auto self = s.lock()) {
                    std::unique_lock<std::mutex> lock{self->m_mutex};
                    self->pushBuffer(self->m_lastCtx, self->m_lastPath, lock);
                    self->scheduleFlush();
                }
            });
    }

private:
    void pushBuffer(CTXPtr ctx, const boost::filesystem::path &p,
        std::unique_lock<std::mutex> &lock)
    {
        if (m_bufferedSize == 0)
            return;

        auto startPoint = std::chrono::steady_clock::now();
        auto buffers =
            std::make_shared<decltype(m_buffers)>(std::move(m_buffers));

        auto sentSize = m_bufferedSize;
        m_pendingConfirmation += sentSize;
        m_bufferedSize = 0;
        m_buffers = {};

        auto callback = [
            =, b = buffers, s = std::weak_ptr<WriteBuffer>(shared_from_this())
        ](std::size_t wrote, const std::error_code &ec) mutable
        {
            auto self = s.lock();
            if (!self)
                return;

            if (!ec) {
                auto duration =
                    std::chrono::duration_cast<std::chrono::nanoseconds>(
                        std::chrono::steady_clock::now() - startPoint)
                        .count();
                if (duration > 0) {
                    auto bandwidth = wrote * 1000000000 / duration;
                    std::lock_guard<std::mutex> guard{m_mutex};
                    m_bps = (m_bps * 1 + bandwidth * 2) / 3;
                }

                m_readCache->clear();
            }

            std::lock_guard<std::mutex> guard{m_mutex};
            if (ec)
                m_lastError = ec;

            m_pendingConfirmation -= sentSize;
            m_confirmationCondition.notify_all();
        };

        std::vector<std::pair<off_t, asio::const_buffer>> bufBuffers;
        std::transform(buffers->begin(), buffers->end(),
            std::back_inserter(bufBuffers), [](const auto &e) {
                return std::make_pair(e.first, asio::buffer(e.second));
            });

        m_helper.ash_multiwrite(
            std::move(ctx), p, std::move(bufBuffers), std::move(callback));

        while (!m_lastError && m_pendingConfirmation > confirmThreshold())
            if (m_confirmationCondition.wait_for(
                    lock, std::chrono::minutes{2}) == std::cv_status::timeout)
                m_lastError = std::make_error_code(std::errc::timed_out);
    }

    std::size_t flushThreshold()
    {
        return std::min(
            m_maxWriteChunkSize, std::max(m_minWriteChunkSize, 2 * m_bps));
    }

    std::size_t confirmThreshold() { return 6 * flushThreshold(); }

    void emplace(const off_t offset, asio::const_buffer buf)
    {
        std::string data{
            asio::buffer_cast<const char *>(buf), asio::buffer_size(buf)};
        m_buffers.emplace_back(offset, std::move(data));
    };

    void append(asio::const_buffer buf)
    {
        auto &data = m_buffers.back().second;

        const auto oldSize = data.size();
        data.resize(oldSize + asio::buffer_size(buf));
        auto dataBuffer = asio::buffer(&data[0], data.size());
        asio::buffer_copy(dataBuffer + oldSize, buf);
    }

    off_t nextOffset() const
    {
        return m_buffers.back().first + m_buffers.back().second.size();
    }

    void throwLastError()
    {
        if (m_lastError) {
            auto err = m_lastError;
            m_lastError = {};
            throw std::system_error{err};
        }
    }

    std::size_t m_minWriteChunkSize;
    std::size_t m_maxWriteChunkSize;
    std::chrono::seconds m_flushWriteAfter;

    IStorageHelper &m_helper;
    Scheduler &m_scheduler;
    std::shared_ptr<ReadCache> m_readCache;

    std::function<void()> m_cancelFlushSchedule;
    CTXPtr m_lastCtx;
    boost::filesystem::path m_lastPath;

    std::size_t m_bufferedSize = 0;
    std::vector<std::pair<off_t, std::string>> m_buffers;
    std::error_code m_lastError;
    std::size_t m_bps = 0;

    std::mutex m_mutex;
    std::size_t m_pendingConfirmation = 0;
    std::condition_variable m_confirmationCondition;
};

} // namespace proxyio
} // namespace helpers
} // namespace one

#endif // HELPERS_BUFFERING_WRITE_BUFFER_H
