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
#include "helpers/IStorageHelper.h"
#include "messages/proxyio/remoteData.h"
#include "messages/proxyio/remoteRead.h"
#include "scheduler.h"

#include <atomic>
#include <chrono>
#include <cstdint>

namespace one {
namespace helpers {
namespace buffering {

class ReadCache {
public:
    ReadCache(std::size_t minReadChunkSize, std::size_t maxReadChunkSize,
        std::chrono::seconds readAheadFor, IStorageHelper &helper)
        : m_minReadChunkSize{minReadChunkSize}
        , m_maxReadChunkSize{maxReadChunkSize}
        , m_readAheadFor{readAheadFor}
        , m_helper{helper}
    {
    }

    asio::mutable_buffer read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, const off_t offset)
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
            m_pendingRead = download(ctx, p, m_pendingReadOffset, blockSize());

            if (offset <
                static_cast<off_t>(m_lastReadOffset + m_lastRead.size())) {
                auto cacheOffset = offset - m_lastReadOffset;
                auto copied = asio::buffer_copy(
                    buf, asio::buffer(m_lastRead) + cacheOffset);
                return asio::buffer(buf, copied);
            }
        }

        auto size = asio::buffer_size(buf);
        auto future = download(ctx, p, offset, size);
        m_pendingReadOffset = offset + size;
        m_pendingRead = download(ctx, p, m_pendingReadOffset, blockSize());

        m_lastReadOffset = offset;
        m_lastRead = readFuture(future);
        m_lastCacheRefresh = std::chrono::steady_clock::now();

        auto copied = asio::buffer_copy(buf, asio::buffer(m_lastRead));
        return asio::buffer(buf, copied);
    }

    void clear() { m_clear = true; }

private:
    std::future<std::string> download(CTXPtr ctx,
        const boost::filesystem::path &p, const off_t offset,
        const std::size_t block)
    {
        auto promise = std::make_shared<std::promise<std::string>>();
        auto future = promise->get_future();
        auto startPoint = std::chrono::steady_clock::now();
        auto stringBuffer = std::make_shared<std::string>(block, '\0');

        auto callback = [=](
            asio::mutable_buffer buf, const std::error_code &ec) mutable {
            if (ec) {
                promise->set_exception(
                    std::make_exception_ptr(std::system_error{ec}));
            }
            else {
                auto bandwidth = asio::buffer_size(buf) * 1000 /
                    std::chrono::duration_cast<std::chrono::milliseconds>(
                        std::chrono::steady_clock::now() - startPoint)
                        .count();

                std::lock_guard<std::mutex> guard{m_mutex};
                m_bps = (m_bps * 1 + bandwidth * 2) / 3;

                stringBuffer->resize(asio::buffer_size(buf));
                promise->set_value(std::move(*stringBuffer));
            };
        };

        auto buf = asio::buffer(&(*stringBuffer)[0], stringBuffer->size());
        m_helper.ash_read(std::move(ctx), p, buf, offset, std::move(callback));

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

    const std::size_t m_minReadChunkSize;
    const std::size_t m_maxReadChunkSize;
    const std::chrono::seconds m_readAheadFor;
    IStorageHelper &m_helper;

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

#endif // HELPERS_BUFFERING_READ_CACHE_H
