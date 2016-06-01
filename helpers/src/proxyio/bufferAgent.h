/**
 * @file bufferAgent.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_PROXYIO_BUFFER_AGENT_H
#define HELPERS_PROXYIO_BUFFER_AGENT_H

#include "readCache.h"
#include "writeBuffer.h"

#include "communication/communicator.h"
#include "helpers/IStorageHelper.h"
#include "scheduler.h"

#include <glog/logging.h>
#include <tbb/concurrent_hash_map.h>

#include <memory>

namespace one {
namespace helpers {
namespace proxyio {

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

class BufferAgent {
    template <typename T> using GeneralCallback = helpers::GeneralCallback<T>;
    using FileKey = std::pair<std::string, std::string>;

public:
    BufferAgent(BufferLimits bufferLimits,
        communication::Communicator &communicator, Scheduler &scheduler)
        : m_bufferLimits{bufferLimits}
        , m_communicator{communicator}
        , m_scheduler{scheduler}
    {
    }

    int open(const std::string &storageId, const std::string &fileId,
        const std::unordered_map<std::string, std::string> &params)
    {
        auto fileKey = makeKey(storageId, fileId);

        typename decltype(m_bufferCounters)::accessor acc;
        if (m_bufferCounters.insert(acc, fileKey)) {
            const auto &bl = m_bufferLimits;

            auto readCache = std::make_shared<ReadCache>(storageId, fileId,
                params, bl.minReadChunkSize, bl.maxReadChunkSize,
                bl.readAheadFor, m_communicator, m_scheduler);

            auto writeBuffer = std::make_shared<WriteBuffer>(storageId, fileId,
                params, bl.minWriteChunkSize, bl.maxWriteChunkSize,
                bl.flushWriteAfter, m_communicator, m_scheduler, readCache);

            acc->second = 1;
            m_readCaches.insert({fileKey, std::move(readCache)});
            m_writeBuffers.insert({fileKey, std::move(writeBuffer)});
        }
        else
            ++acc->second;

        return 0;
    }

    asio::mutable_buffer read(const std::string &storageId,
        const std::string &fileId, asio::mutable_buffer buf, off_t offset)
    {
        typename decltype(m_readCaches)::accessor acc;
        m_readCaches.find(acc, makeKey(storageId, fileId));
        return acc->second->read(buf, offset);
    }

    std::size_t write(const std::string &storageId, const std::string &fileId,
        asio::const_buffer buf, off_t offset)
    {
        typename decltype(m_writeBuffers)::accessor acc;
        m_writeBuffers.find(acc, makeKey(storageId, fileId));
        return acc->second->write(buf, offset);
    }

    void flush(const std::string &storageId, const std::string &fileId)
    {
        typename decltype(m_writeBuffers)::accessor writeAcc;
        m_writeBuffers.find(writeAcc, makeKey(storageId, fileId));
        writeAcc->second->flush();

        typename decltype(m_readCaches)::accessor readAcc;
        m_readCaches.find(readAcc, makeKey(storageId, fileId));
        readAcc->second->clear();
    }

    void fsync(const std::string &storageId, const std::string &fileId)
    {
        typename decltype(m_writeBuffers)::accessor acc;
        m_writeBuffers.find(acc, makeKey(storageId, fileId));
        acc->second->fsync();
    }

    void release(const std::string &storageId, const std::string &fileId)
    {
        auto fileKey = makeKey(storageId, fileId);

        typename decltype(m_bufferCounters)::accessor acc;
        m_bufferCounters.find(acc, fileKey);
        if (--acc->second == 0) {
            typename decltype(m_writeBuffers)::accessor writeAcc;
            m_writeBuffers.find(writeAcc, fileKey);

            auto finally = [&] {
                m_writeBuffers.erase(writeAcc);
                m_readCaches.erase(fileKey);
                m_bufferCounters.erase(acc);
            };

            try {
                writeAcc->second->fsync();
            }
            catch (...) {
                finally();
                throw;
            }

            finally();
        }
    }

private:
    FileKey makeKey(std::string storageId, std::string fileId)
    {
        return {std::move(storageId), std::move(fileId)};
    }

    BufferLimits m_bufferLimits;
    communication::Communicator &m_communicator;
    Scheduler &m_scheduler;

    tbb::concurrent_hash_map<FileKey, std::size_t> m_bufferCounters;
    tbb::concurrent_hash_map<FileKey, std::shared_ptr<ReadCache>> m_readCaches;
    tbb::concurrent_hash_map<FileKey, std::shared_ptr<WriteBuffer>>
        m_writeBuffers;
};

} // namespace proxyio
} // namespace helpers
} // namespace one

#endif // HELPERS_PROXYIO_BUFFER_AGENT_H
