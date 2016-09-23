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

namespace {
template <typename CbValue> struct TaskCtx {
    TaskCtx(std::string fileId_, GeneralCallback<CbValue> _callback)
        : prefix{std::move(fileId_)}
        , parts{0}
        , callback{std::move(_callback)} {};

    std::string prefix;
    std::atomic<std::size_t> parts;
    std::error_code code = SUCCESS_CODE;
    std::mutex mutex;
    GeneralCallback<CbValue> callback;
};
}

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
    asio::post(
        m_service, [ =, ctx = std::move(ctx), callback = std::move(callback) ] {
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
    auto prefix = p.string();

    asio::post(m_service, [
        =, ctx = std::move(ctx), prefix = std::move(prefix),
        buf = std::move(buf), callback = std::move(callback)
    ]() mutable {
        auto fileSize = m_helper->getObjectsSize(ctx, prefix, m_blockSize);
        if (offset >= fileSize) {
            callback(asio::buffer(buf, 0), SUCCESS_CODE);
        }
        else {
            readBlocks(std::move(ctx), std::move(prefix), std::move(buf),
                offset, fileSize, std::move(callback));
        }
    });
}

void KeyValueAdapter::ash_write(CTXPtr ctx, const boost::filesystem::path &p,
    asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t> callback)
{
    auto size = asio::buffer_size(buf);

    if (size == 0)
        return callback(0, SUCCESS_CODE);

    auto prefix = p.string();
    auto blockId = getBlockId(offset);
    auto blockOffset = getBlockOffset(offset);
    auto blocksToWrite = (size + blockOffset + m_blockSize - 1) / m_blockSize;
    auto writeCtx = std::make_shared<TaskCtx<std::size_t>>(
        std::move(prefix), std::move(callback));

    for (std::size_t bufOffset = 0; bufOffset < size;
         blockOffset = 0, ++blockId) {
        auto blockSize =
            std::min<std::size_t>(m_blockSize - blockOffset, size - bufOffset);

        asio::post(m_service, [=]() mutable {
            try {
                writeBlock(std::move(ctx), writeCtx->prefix, buf, bufOffset,
                    blockId, blockOffset, blockSize);
            }
            catch (const std::system_error &e) {
                std::lock_guard<std::mutex> guard{writeCtx->mutex};
                if (!writeCtx->code) {
                    logError("write", e);
                    writeCtx->code = e.code();
                }
            }
            if (++writeCtx->parts == blocksToWrite) {
                writeCtx->callback(size, writeCtx->code);
            }
        });

        bufOffset += blockSize;
    }
}

void KeyValueAdapter::ash_truncate(CTXPtr ctx, const boost::filesystem::path &p,
    off_t size, VoidCallback callback)
{
    asio::post(
        m_service, [ =, ctx = std::move(ctx), callback = std::move(callback) ] {
            try {
                auto prefix = p.string();
                auto blockId = getBlockId(size);
                auto blockOffset = getBlockOffset(size);

                auto keys = m_helper->listObjects(ctx, p.string());
                std::vector<std::string> keysToDelete{};

                for (auto &key : keys) {
                    auto objectId = m_helper->getObjectId(key);

                    if (objectId > blockId ||
                        (objectId == blockId && blockOffset == 0)) {
                        keysToDelete.emplace_back(std::move(key));
                    }
                }

                auto key = m_helper->getKey(prefix, blockId);
                auto blockSize = static_cast<std::size_t>(blockOffset);

                if (blockSize == 0 && blockId > 0) {
                    key = m_helper->getKey(prefix, blockId - 1);
                    blockSize = m_blockSize;
                }

                if (blockSize > 0 || blockId > 0) {
                    std::vector<char> data(blockSize, '\0');
                    auto blockBuf = asio::buffer(data);

                    Locks::accessor acc;
                    m_locks.insert(acc, key);
                    readBlock(ctx, key, blockBuf, 0);
                    m_helper->putObject(ctx, std::move(key), blockBuf);
                    m_locks.erase(acc);
                }

                if (!keysToDelete.empty())
                    m_helper->deleteObjects(ctx, std::move(keysToDelete));

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

void KeyValueAdapter::readBlocks(CTXPtr ctx, std::string prefix,
    asio::mutable_buffer buf, off_t offset, std::size_t fileSize,
    GeneralCallback<asio::mutable_buffer> callback)
{
    buf = asio::buffer(buf, fileSize - offset);
    auto size = asio::buffer_size(buf);

    if (size == 0)
        return callback(asio::buffer(buf, 0), SUCCESS_CODE);

    auto blockId = getBlockId(offset);
    auto blockOffset = getBlockOffset(offset);
    auto blocksToRead = (size + blockOffset + m_blockSize - 1) / m_blockSize;
    auto readCtx = std::make_shared<TaskCtx<asio::mutable_buffer>>(
        std::move(prefix), std::move(callback));

    for (std::size_t bufOffset = 0; bufOffset < size;
         blockOffset = 0, ++blockId) {

        asio::post(m_service, [=]() mutable {
            try {
                readBlock(std::move(ctx), readCtx->prefix, buf, bufOffset,
                    blockId, blockOffset);
            }
            catch (const std::system_error &e) {
                std::lock_guard<std::mutex> guard{readCtx->mutex};
                if (!readCtx->code) {
                    logError("read", e);
                    readCtx->code = e.code();
                }
            }
            if (++readCtx->parts == blocksToRead) {
                readCtx->callback(buf, readCtx->code);
            }
        });

        bufOffset += m_blockSize - blockOffset;
    }
}

std::size_t KeyValueAdapter::readBlock(CTXPtr ctx, const std::string &prefix,
    asio::mutable_buffer buf, std::size_t bufOffset, uint64_t blockId,
    off_t blockOffset)
{
    auto blockBuf = asio::buffer(buf + bufOffset, m_blockSize);
    auto key = m_helper->getKey(prefix, blockId);
    Locks::accessor acc;
    m_locks.insert(acc, key);
    readBlock(ctx, std::move(key), blockBuf, blockOffset);
    m_locks.erase(key);
    return asio::buffer_size(blockBuf);
}

asio::mutable_buffer KeyValueAdapter::readBlock(
    CTXPtr ctx, std::string key, asio::mutable_buffer buf, off_t offset)
{
    try {
        return m_helper->getObject(ctx, std::move(key), buf, offset);
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

void KeyValueAdapter::writeBlock(CTXPtr ctx, const std::string &prefix,
    asio::const_buffer buf, std::size_t bufOffset, uint64_t blockId,
    off_t blockOffset, std::size_t blockSize)
{
    auto key = m_helper->getKey(prefix, blockId);
    Locks::accessor acc;
    m_locks.insert(acc, key);

    if (blockSize != m_blockSize) {
        std::vector<char> data(m_blockSize, '\0');
        auto blockBuf = asio::buffer(data);
        auto fetchedBuf = readBlock(ctx, key, blockBuf, 0);

        auto targetBlockSize =
            std::max(asio::buffer_size(fetchedBuf), blockOffset + blockSize);

        blockBuf = asio::buffer(blockBuf, targetBlockSize);
        asio::buffer_copy(blockBuf + blockOffset, buf + bufOffset);

        m_helper->putObject(ctx, std::move(key), blockBuf);
    }
    else {
        auto blockBuf = asio::buffer(buf + bufOffset, blockSize);
        m_helper->putObject(ctx, std::move(key), blockBuf);
    }

    m_locks.erase(acc);
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
