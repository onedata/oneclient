/**
 * @file readCache.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_PROXYIO_READ_CACHE_H
#define HELPERS_PROXYIO_READ_CACHE_H

#include "communication/communicator.h"
#include "helpers/IStorageHelper.h"
#include "messages/proxyio/remoteData.h"
#include "messages/proxyio/remoteRead.h"
#include "scheduler.h"

#include <atomic>
#include <chrono>
#include <cstdint>

namespace one {
namespace helpers {
namespace proxyio {

class ReadCache {
public:
    ReadCache(std::string storageId, std::string fileId,
        std::unordered_map<std::string, std::string> params,
        std::size_t minReadChunkSize, std::size_t maxReadChunkSize,
        std::chrono::seconds readAheadFor,
        communication::Communicator &communicator, Scheduler &scheduler)
        : m_storageId{std::move(storageId)}
        , m_fileId{std::move(fileId)}
        , m_params{std::move(params)}
        , m_minReadChunkSize{minReadChunkSize}
        , m_maxReadChunkSize{maxReadChunkSize}
        , m_readAheadFor{readAheadFor}
        , m_communicator{communicator}
    {
    }

    asio::mutable_buffer read(asio::mutable_buffer buf, const off_t offset)
    {
        if (m_clear ||
            m_lastCacheRefresh + std::chrono::seconds{5} <
                std::chrono::steady_clock::now()) {
            m_lastRead.clear();
            m_pendingRead = {};
            m_clear = false;
        }

        if (m_lastReadOffset <= offset &&
            offset < static_cast<off_t>(m_lastReadOffset + m_lastRead.size())) {
            auto cacheOffset = offset - m_lastReadOffset;
            auto copied =
                asio::buffer_copy(buf, asio::buffer(m_lastRead) + cacheOffset);
            return asio::buffer(buf, copied);
        }

        if (m_pendingRead.valid() && m_pendingReadOffset <= offset) {
            m_lastReadOffset = m_pendingReadOffset;
            m_lastRead = readFuture(m_pendingRead);
            m_lastCacheRefresh = std::chrono::steady_clock::now();

            m_pendingReadOffset = m_lastReadOffset + m_lastRead.size();
            m_pendingRead = download(m_pendingReadOffset, blockSize());

            if (offset <
                static_cast<off_t>(m_lastReadOffset + m_lastRead.size())) {
                auto cacheOffset = offset - m_lastReadOffset;
                auto copied = asio::buffer_copy(
                    buf, asio::buffer(m_lastRead) + cacheOffset);
                return asio::buffer(buf, copied);
            }
        }

        auto size = asio::buffer_size(buf);
        auto future = download(offset, size);
        m_pendingReadOffset = offset + size;
        m_pendingRead = download(m_pendingReadOffset, blockSize());

        m_lastReadOffset = offset;
        m_lastRead = readFuture(future);
        m_lastCacheRefresh = std::chrono::steady_clock::now();

        auto copied = asio::buffer_copy(buf, asio::buffer(m_lastRead));
        return asio::buffer(buf, copied);
    }

    void clear() { m_clear = true; }

private:
    std::future<std::string> download(
        const off_t offset, const std::size_t block)
    {
        messages::proxyio::RemoteRead msg{
            m_params, m_storageId, m_fileId, offset, block};

        auto promise = std::make_shared<std::promise<std::string>>();
        auto future = promise->get_future();
        auto startPoint = std::chrono::steady_clock::now();

        auto callback = [=](const std::error_code &ec,
            std::unique_ptr<messages::proxyio::RemoteData> rd) mutable {
            if (ec) {
                promise->set_exception(
                    std::make_exception_ptr(std::system_error{ec}));
            }
            else {
                auto bandwidth = rd->mutableData().size() * 1000 /
                    std::chrono::duration_cast<std::chrono::milliseconds>(
                        std::chrono::steady_clock::now() - startPoint)
                        .count();

                std::lock_guard<std::mutex> guard{m_mutex};
                m_bps = (m_bps * 1 + bandwidth * 2) / 3;
                promise->set_value(std::move(rd->mutableData()));
            }
        };

        m_communicator.communicate<messages::proxyio::RemoteData>(
            std::move(msg), std::move(callback));

        return future;
    }

    std::size_t blockSize()
    {
        return std::min(
                   m_maxReadChunkSize, std::max(m_minReadChunkSize, m_bps)) *
            m_readAheadFor.count();
    }

    std::string readFuture(std::future<std::string> &future)
    {
        // 500ms + 2ms for each byte (minimum of 500B/s);
        std::chrono::milliseconds timeout{5000 + blockSize() * 2};
        return communication::wait(future, timeout);
    }

    std::string m_storageId;
    std::string m_fileId;
    std::unordered_map<std::string, std::string> m_params;
    const std::size_t m_minReadChunkSize;
    const std::size_t m_maxReadChunkSize;
    const std::chrono::seconds m_readAheadFor;
    communication::Communicator &m_communicator;

    std::mutex m_mutex;
    std::size_t m_bps = 0;

    off_t m_lastReadOffset = 0;
    std::string m_lastRead;
    off_t m_pendingReadOffset = 0;
    std::future<std::string> m_pendingRead;
    std::atomic<bool> m_clear{false};
    std::chrono::steady_clock::time_point m_lastCacheRefresh{};
};

} // namespace proxyio
} // namespace helpers
} // namespace one

#endif // HELPERS_PROXYIO_READ_CACHE_H
