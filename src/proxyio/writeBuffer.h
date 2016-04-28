/**
 * @file writeBuffer.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_PROXYIO_WRITE_BUFFER_H
#define HELPERS_PROXYIO_WRITE_BUFFER_H

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
#include <mutex>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

namespace one {
namespace helpers {
namespace proxyio {

/**
 * Writes to a single file are serialized by the kernel, so this class doesn't
 * have to be thread-safe. A mutex is introduced to synchronize between writes
 * and scheduled flush.
 */
class WriteBuffer {
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
    WriteBuffer(std::string storageId, std::string fileId,
        std::unordered_map<std::string, std::string> params,
        const std::size_t minWriteChunkSize,
        const std::size_t maxWriteChunkSize,
        const std::chrono::seconds flushWriteAfter,
        communication::Communicator &communicator, Scheduler &scheduler,
        std::shared_ptr<ReadCache> readCache)
        : m_storageId{std::move(storageId)}
        , m_fileId{std::move(fileId)}
        , m_params{std::move(params)}
        , m_minWriteChunkSize{minWriteChunkSize}
        , m_maxWriteChunkSize{maxWriteChunkSize}
        , m_flushWriteAfter{flushWriteAfter}
        , m_scheduler{scheduler}
        , m_communicator{communicator}
        , m_readCache{readCache}
    {
        scheduleFlush();
    }

    ~WriteBuffer()
    {
        std::lock_guard<std::mutex> guard{m_mutex};
        m_cancelFlushSchedule();
    }

    std::size_t write(asio::const_buffer buf, const off_t offset)
    {
        std::unique_lock<std::mutex> lock{m_mutex};
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
            pushBuffer(lock);
        }

        return asio::buffer_size(buf);
    }

    void flush()
    {
        std::unique_lock<std::mutex> lock{m_mutex};
        throwLastError();

        pushBuffer(lock);
    }

    void fsync()
    {
        std::unique_lock<std::mutex> lock{m_mutex};
        throwLastError();
        pushBuffer(lock);

        while (!m_lastError && m_pendingConfirmation > 0)
            if (m_confirmationCondition.wait_for(
                    lock, std::chrono::minutes{2}) == std::cv_status::timeout)
                m_lastError = std::make_error_code(std::errc::timed_out);

        throwLastError();
    }

    void release(VoidCallback callback) { fsync(); }

private:
    void scheduleFlush()
    {
        m_cancelFlushSchedule = m_scheduler.schedule(m_flushWriteAfter, [this] {
            std::unique_lock<std::mutex> lock_{m_mutex};
            pushBuffer(lock_);
            scheduleFlush();
        });
    }

    void pushBuffer(std::unique_lock<std::mutex> &lock)
    {
        if (m_bufferedSize == 0)
            return;

        // Possible todo: minimizing data sent
        messages::proxyio::RemoteWrite msg{
            m_params, m_storageId, m_fileId, std::move(m_buffers)};

        auto sentSize = m_bufferedSize;
        m_pendingConfirmation += sentSize;
        m_bufferedSize = 0;
        m_buffers = {};

        auto startPoint = std::chrono::steady_clock::now();

        m_communicator.communicate<messages::proxyio::RemoteWriteResult>(
            std::move(msg), [=](auto &ec, auto rd) {

                if (!ec) {
                    auto bandwidth = rd->wrote() * 1000 /
                        std::chrono::duration_cast<std::chrono::milliseconds>(
                            std::chrono::steady_clock::now() - startPoint)
                            .count();

                    std::lock_guard<std::mutex> guard{this->m_mutex};
                    this->m_bps = (m_bps * 1 + bandwidth * 2) / 3;
                    this->m_readCache->clear();
                }

                std::lock_guard<std::mutex> guard{this->m_mutex};
                if (ec)
                    this->m_lastError = ec;

                this->m_pendingConfirmation -= sentSize;
                this->m_confirmationCondition.notify_all();
            });

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

    std::string m_storageId;
    std::string m_fileId;
    std::unordered_map<std::string, std::string> m_params;
    std::size_t m_minWriteChunkSize;
    std::size_t m_maxWriteChunkSize;
    std::chrono::seconds m_flushWriteAfter;

    Scheduler &m_scheduler;
    communication::Communicator &m_communicator;
    std::shared_ptr<ReadCache> m_readCache;

    std::function<void()> m_cancelFlushSchedule;

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

#endif // HELPERS_PROXYIO_WRITE_BUFFER_H
