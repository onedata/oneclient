/**
 * @file keyValueAdapter.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "keyValueAdapter.h"
#include "keyValueHelper.h"
#include "logging.h"

namespace one {
namespace helpers {

KeyValueAdapter::KeyValueAdapter(std::unique_ptr<KeyValueHelper> helper,
    asio::io_service &service, Locks &locks, std::size_t blockSize)
    : m_helper{std::move(helper)}
    , m_service{service}
    , m_locks{locks}
    , m_blockSize{blockSize}
{
}

CTXPtr KeyValueAdapter::createCTX(
    std::unordered_map<std::string, std::string> params)
{
    return m_helper->createCTX(std::move(params));
}

void KeyValueAdapter::ash_unlink(
    CTXPtr ctx, const boost::filesystem::path &p, VoidCallback callback)
{
    m_service.post([ =, ctx = std::move(ctx), callback = std::move(callback) ] {
        try {
            auto keys = m_helper->listObjects(ctx, p.string());
            m_helper->deleteObjects(ctx, std::move(keys));
            callback(SUCCESS_CODE);
        }
        catch (const std::system_error &e) {
            logError("unlink", e);
            callback(e.code());
        }
    });
}

void KeyValueAdapter::ash_read(CTXPtr ctx, const boost::filesystem::path &p,
    asio::mutable_buffer buf, off_t offset,
    GeneralCallback<asio::mutable_buffer> callback)
{
    m_service.post([
        =, ctx = std::move(ctx), buf = std::move(buf),
        callback = std::move(callback)
    ] {
        try {
            std::size_t bufOffset = 0;
            auto fileSize =
                m_helper->getObjectsSize(ctx, p.string(), m_blockSize);

            if (offset >= fileSize)
                return callback(asio::buffer(buf, 0), SUCCESS_CODE);

            auto size = std::min(asio::buffer_size(buf),
                static_cast<std::size_t>(fileSize - offset));
            auto blockId = getBlockId(offset);
            auto blockOffset = getBlockOffset(offset);

            while (bufOffset < size) {
                auto blockSize =
                    std::min<std::size_t>(m_blockSize - blockOffset,
                        static_cast<std::size_t>(size - bufOffset));
                auto data = asio::buffer_cast<char *>(buf) + bufOffset;
                std::memset(data, 0, blockSize);
                asio::mutable_buffer blockBuf{data, blockSize};
                auto key = m_helper->getKey(p.string(), blockId);
                Locks::accessor acc;
                m_locks.insert(acc, key);
                getBlock(ctx, std::move(key), std::move(blockBuf), blockOffset);
                m_locks.erase(key);

                ++blockId;
                blockOffset = 0;
                bufOffset += blockSize;
            }

            callback(asio::buffer(buf, size), SUCCESS_CODE);
        }
        catch (const std::system_error &e) {
            logError("read", e);
            callback(asio::mutable_buffer{}, e.code());
        }
    });
}

void KeyValueAdapter::ash_write(CTXPtr ctx, const boost::filesystem::path &p,
    asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t> callback)
{
    m_service.post([
        =, ctx = std::move(ctx), buf = std::move(buf),
        callback = std::move(callback)
    ] {
        try {
            std::size_t bufOffset = 0;
            auto size = asio::buffer_size(buf);
            auto blockId = getBlockId(offset);
            auto blockOffset = getBlockOffset(offset);

            while (bufOffset < size) {
                auto blockSize =
                    std::min<std::size_t>(m_blockSize - blockOffset,
                        static_cast<std::size_t>(size - bufOffset));

                auto key = m_helper->getKey(p.string(), blockId);
                Locks::accessor acc;
                m_locks.insert(acc, key);

                if (blockSize != m_blockSize) {
                    std::vector<char> data(m_blockSize, '\0');
                    asio::mutable_buffer blockBuf{data.data(), m_blockSize};
                    auto objectSize = blockOffset + blockSize;

                    blockBuf = getBlock(ctx, key, std::move(blockBuf), 0);
                    objectSize = std::max(
                        asio::buffer_size(blockBuf), blockOffset + blockSize);

                    std::memcpy(data.data() + blockOffset,
                        asio::buffer_cast<const char *>(buf) + bufOffset,
                        blockSize);

                    m_helper->putObject(ctx, std::move(key),
                        asio::const_buffer(data.data(), objectSize));
                }
                else {
                    auto data =
                        asio::buffer_cast<const char *>(buf) + bufOffset;
                    asio::const_buffer blockBuf{data, blockSize};

                    blockSize = m_helper->putObject(
                        ctx, std::move(key), std::move(blockBuf));
                }

                m_locks.erase(acc);

                ++blockId;
                blockOffset = 0;
                bufOffset += blockSize;
            }
            callback(size, SUCCESS_CODE);
        }
        catch (const std::system_error &e) {
            logError("write", e);
            callback(0, e.code());
        }
    });
}

void KeyValueAdapter::ash_truncate(CTXPtr ctx, const boost::filesystem::path &p,
    off_t size, VoidCallback callback)
{
    m_service.post([ =, ctx = std::move(ctx), callback = std::move(callback) ] {
        try {
            auto blockId = getBlockId(size);
            auto blockOffset = getBlockOffset(size);

            auto keys = m_helper->listObjects(ctx, p.string());
            std::vector<std::string> keysToDelete{};

            for (const auto &key : keys) {
                auto objectId = m_helper->getObjectId(key);

                if (objectId > blockId ||
                    (objectId == blockId && blockOffset == 0)) {
                    keysToDelete.emplace_back(key);
                }
            }

            std::string key = m_helper->getKey(p.string(), blockId);
            std::size_t blockSize = static_cast<std::size_t>(blockOffset);

            if (blockSize == 0 && blockId > 0) {
                key = m_helper->getKey(p.string(), blockId - 1);
                blockSize = m_blockSize;
            }

            if (blockSize > 0 || blockId > 0) {
                std::vector<char> data(blockSize);
                asio::mutable_buffer blockBuf{data.data(), blockSize};

                Locks::accessor acc;
                m_locks.insert(acc, key);
                getBlock(ctx, key, blockBuf, 0);
                blockSize = m_helper->putObject(ctx, std::move(key),
                    asio::const_buffer(data.data(), blockSize));
                m_locks.erase(acc);
            }

            if (!keysToDelete.empty())
                m_helper->deleteObjects(ctx, keysToDelete);

            callback(SUCCESS_CODE);
        }
        catch (const std::system_error &e) {
            logError("truncate", e);
            callback(e.code());
        }
    });
}

uint64_t KeyValueAdapter::getBlockId(off_t offset)
{
    return offset / m_blockSize;
}

off_t KeyValueAdapter::getBlockOffset(off_t offset)
{
    return offset - getBlockId(offset) * m_blockSize;
}

asio::mutable_buffer KeyValueAdapter::getBlock(
    CTXPtr ctx, std::string key, asio::mutable_buffer buf, off_t offset)
{

    try {
        return m_helper->getObject(ctx, std::move(key), std::move(buf), offset);
    }
    catch (const std::system_error &e) {
        if (e.code().value() == ENOENT) {
            return asio::mutable_buffer{};
        }
        else {
            throw;
        }
    }
}

void KeyValueAdapter::logError(
    std::string operation, const std::system_error &error)
{
    LOG(ERROR) << "Operation '" << operation
               << "' failed due to: " << error.what()
               << " (code: " << error.code().value() << ")";
}

} // namespace helpers
} // namespace one
