/**
 * @file keyValueAdapter.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "keyValueAdapter.h"
#include "keyValueHelper.h"

#include <cstring>
#include <sstream>

namespace one {
namespace helpers {

constexpr auto BLOCK_DELIMITER = "/";

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
        }
        catch (const std::system_error &e) {
            return callback(e.code());
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
        std::size_t bufOffset = 0;
        std::size_t bufSize = 0;
        auto size = asio::buffer_size(buf);
        auto blockId = getBlockId(offset);
        auto blockOffset = getBlockOffset(offset);

        while (bufOffset < size) {
            auto blockSize = std::min<std::size_t>(m_blockSize - blockOffset,
                static_cast<std::size_t>(size - bufOffset));
            auto data = asio::buffer_cast<char *>(buf) + bufOffset;
            std::memset(data, 0, blockSize);
            asio::mutable_buffer blockBuf{data, blockSize};

            try {
                std::string key{getKey(p, blockId)};

                Locks::accessor acc;
                m_locks.insert(acc, key);
                blockBuf = m_helper->getObject(
                    ctx, std::move(key), std::move(blockBuf), blockOffset);
                m_locks.erase(acc);

                bufSize = bufOffset + asio::buffer_size(blockBuf);
            }
            catch (const std::system_error &e) {
                if (e.code().value() != ENOENT) {
                    return callback(asio::mutable_buffer{}, e.code());
                }
            }

            ++blockId;
            blockOffset = 0;
            bufOffset += blockSize;
        }

        callback(asio::buffer(buf, bufSize), SUCCESS_CODE);
    });
}

void KeyValueAdapter::ash_write(CTXPtr ctx, const boost::filesystem::path &p,
    asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t> callback)
{
    m_service.post([
        =, ctx = std::move(ctx), buf = std::move(buf),
        callback = std::move(callback)
    ] {
        std::size_t bufOffset = 0;
        std::size_t bufSize = 0;
        auto size = asio::buffer_size(buf);
        auto blockId = getBlockId(offset);
        auto blockOffset = getBlockOffset(offset);

        while (bufOffset < size) {
            auto blockSize = std::min<std::size_t>(m_blockSize - blockOffset,
                static_cast<std::size_t>(size - bufOffset));

            try {
                std::string key{getKey(p, blockId)};

                if (blockSize != m_blockSize) {
                    std::vector<char> data(m_blockSize);
                    asio::mutable_buffer blockBuf{data.data(), m_blockSize};

                    Locks::accessor acc;
                    m_locks.insert(acc, key);
                    blockBuf = m_helper->getObject(ctx, key, blockBuf, 0);
                    std::memcpy(data.data() + blockOffset,
                        asio::buffer_cast<const char *>(buf) + bufOffset,
                        blockSize);
                    blockSize = m_helper->putObject(ctx, std::move(key),
                        asio::const_buffer(
                            data.data(), std::min(asio::buffer_size(blockBuf),
                                             blockOffset + blockSize)));
                    m_locks.erase(acc);

                    bufSize += blockSize - blockOffset;
                }
                else {
                    auto data =
                        asio::buffer_cast<const char *>(buf) + bufOffset;
                    asio::const_buffer blockBuf{data, blockSize};

                    Locks::accessor acc;
                    m_locks.insert(acc, key);
                    blockSize = m_helper->putObject(
                        ctx, std::move(key), std::move(blockBuf));
                    m_locks.erase(acc);

                    bufSize += blockSize;
                }
            }
            catch (const std::system_error &e) {
                return callback(0, e.code());
            }

            ++blockId;
            blockOffset = 0;
            bufOffset += blockSize;
        }

        callback(bufSize, SUCCESS_CODE);
    });
}

void KeyValueAdapter::ash_truncate(CTXPtr ctx, const boost::filesystem::path &p,
    off_t size, VoidCallback callback)
{
    m_service.post([ =, ctx = std::move(ctx), callback = std::move(callback) ] {
        try {
            auto blockId = getBlockId(size);

            auto keys = m_helper->listObjects(ctx, p.string());
            std::vector<std::string> keysToDelete{};

            while (!keys.empty() && getBlockId(keys.back()) > blockId) {
                keysToDelete.emplace_back(keys.back());
                keys.pop_back();
            }

            if (!keys.empty() && getBlockId(keys.back()) == blockId) {
                auto blockSize = static_cast<std::size_t>(getBlockOffset(size));
                if (blockSize == 0) {
                    keysToDelete.emplace_back(keys.back());
                }
                else {
                    std::string key{getKey(p, blockId)};
                    std::vector<char> data(blockSize);
                    asio::mutable_buffer blockBuf{data.data(), blockSize};

                    Locks::accessor acc;
                    m_locks.insert(acc, key);
                    blockBuf = m_helper->getObject(ctx, key, blockBuf, 0);
                    blockSize = m_helper->putObject(
                        ctx, std::move(key), asio::const_buffer(data.data(),
                                                 asio::buffer_size(blockBuf)));
                    m_locks.erase(acc);
                }
            }

            m_helper->deleteObjects(ctx, keysToDelete);
        }
        catch (const std::system_error &e) {
            return callback(e.code());
        }
    });
}

uint64_t KeyValueAdapter::getBlockId(off_t offset)
{
    return offset / m_blockSize;
}

uint64_t KeyValueAdapter::getBlockId(std::string key)
{
    auto pos = key.find_last_of(BLOCK_DELIMITER);
    return std::stoull(key.substr(pos + 1));
}

off_t KeyValueAdapter::getBlockOffset(off_t offset)
{
    return offset - getBlockId(offset) * m_blockSize;
}

std::string KeyValueAdapter::getKey(
    const boost::filesystem::path &path, uint64_t blockId)
{
    std::stringstream ss;
    ss << path << BLOCK_DELIMITER << blockId;
    return ss.str();
}

} // namespace helpers
} // namespace one
