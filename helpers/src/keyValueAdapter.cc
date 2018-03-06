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

namespace {

uint64_t getBlockId(off_t offset, std::size_t blockSize)
{
    LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(blockSize);

    return offset / blockSize;
}

off_t getBlockOffset(off_t offset, std::size_t blockSize)
{
    LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(blockSize);

    return offset - getBlockId(offset, blockSize) * blockSize;
}

void logError(const folly::fbstring &operation, const std::system_error &error)
{
    LOG(ERROR) << "Operation '" << operation
               << "' failed due to: " << error.what()
               << " (code: " << error.code().value() << ")";
}

folly::IOBufQueue readBlock(
    const std::shared_ptr<one::helpers::KeyValueHelper> &helper,
    const folly::fbstring &key, const off_t offset, const std::size_t size)
{
    LOG_FCALL() << LOG_FARG(key) << LOG_FARG(offset) << LOG_FARG(size);

    try {
        return helper->getObject(key, offset, size);
    }
    catch (const std::system_error &e) {
        if (e.code().value() == ENOENT)
            return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};

        throw;
    }
}

folly::IOBufQueue fillToSize(folly::IOBufQueue buf, const std::size_t size)
{
    LOG_FCALL() << LOG_FARG(buf.chainLength()) << LOG_FARG(size);

    if (buf.chainLength() < size) {
        const std::size_t fillLength = size - buf.chainLength();
        char *data = static_cast<char *>(buf.allocate(fillLength));
        std::fill(data, data + fillLength, 0);
    }
    return buf;
}

} // namespace

namespace one {
namespace helpers {

KeyValueFileHandle::KeyValueFileHandle(folly::fbstring fileId,
    std::shared_ptr<KeyValueHelper> helper, const std::size_t blockSize,
    std::shared_ptr<Locks> locks, std::shared_ptr<folly::Executor> executor)
    : FileHandle{std::move(fileId)}
    , m_helper{std::move(helper)}
    , m_blockSize{blockSize}
    , m_locks{std::move(locks)}
    , m_executor{std::move(executor)}
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(blockSize);
}

folly::Future<folly::IOBufQueue> KeyValueFileHandle::read(
    const off_t offset, const std::size_t size)
{
    LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size);

    return folly::via(
        m_executor.get(), [ this, offset, size, self = shared_from_this() ] {
            const off_t fileSize =
                m_helper->getObjectsSize(m_fileId, m_blockSize);

            if (offset >= fileSize)
                return folly::makeFuture(
                    folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()});

            return readBlocks(offset, size, fileSize);
        });
}

folly::Future<std::size_t> KeyValueFileHandle::write(
    const off_t offset, folly::IOBufQueue buf)
{
    LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(buf.chainLength());

    if (buf.empty())
        return folly::makeFuture<std::size_t>(0);

    return folly::via(m_executor.get(),
        [ this, offset, buf = std::move(buf), self = shared_from_this() ] {
            const auto size = buf.chainLength();
            if (size == 0)
                return folly::makeFuture<std::size_t>(0);

            auto blockId = getBlockId(offset, m_blockSize);
            auto blockOffset = getBlockOffset(offset, m_blockSize);

            folly::fbvector<folly::Future<folly::Unit>> writeFutures;

            for (std::size_t bufOffset = 0; bufOffset < size;
                 blockOffset = 0, ++blockId) {

                const auto blockSize = std::min<std::size_t>(
                    m_blockSize - blockOffset, size - bufOffset);

                auto writeFuture = via(m_executor.get(), [
                    this, iobuf = buf.front()->clone(), blockId, blockOffset,
                    bufOffset, blockSize, self = shared_from_this()
                ]() mutable {
                    folly::IOBufQueue bufq{
                        folly::IOBufQueue::cacheChainLength()};

                    bufq.append(std::move(iobuf));
                    bufq.trimStart(bufOffset);
                    bufq.trimEnd(bufq.chainLength() - blockSize);

                    return writeBlock(std::move(bufq), blockId, blockOffset);
                });

                writeFutures.emplace_back(std::move(writeFuture));

                bufOffset += blockSize;
            }

            return folly::collect(writeFutures)
                .then([size](const std::vector<folly::Unit> &) { return size; })
                .then([](folly::Try<std::size_t> t) {
                    try {
                        return t.value();
                    }
                    catch (const std::system_error &e) {
                        logError("write", e);
                        throw;
                    }
                });
        });
}

const Timeout &KeyValueFileHandle::timeout() { return m_helper->timeout(); }

KeyValueAdapter::KeyValueAdapter(std::shared_ptr<KeyValueHelper> helper,
    std::shared_ptr<folly::Executor> executor, std::size_t blockSize)
    : m_helper{std::move(helper)}
    , m_executor{std::move(executor)}
    , m_locks{std::make_shared<Locks>()}
    , m_blockSize{blockSize}
{
    LOG_FCALL() << LOG_FARG(blockSize);
}

folly::Future<folly::Unit> KeyValueAdapter::unlink(
    const folly::fbstring &fileId)
{
    LOG_FCALL() << LOG_FARG(fileId);

    return folly::via(m_executor.get(), [ fileId, helper = m_helper ] {
        try {
            auto keys = helper->listObjects(fileId);
            helper->deleteObjects(keys);
        }
        catch (const std::system_error &e) {
            logError("unlink", e);
            throw;
        }
    });
}

folly::Future<folly::Unit> KeyValueAdapter::truncate(
    const folly::fbstring &fileId, const off_t size)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(size);

    return folly::via(m_executor.get(), [
        fileId, size, helper = m_helper, locks = m_locks,
        defBlockSize = m_blockSize
    ] {
        try {
            auto blockId = getBlockId(size, defBlockSize);
            auto blockOffset = getBlockOffset(size, defBlockSize);

            auto keys = helper->listObjects(fileId);

            folly::fbvector<folly::fbstring> keysToDelete;
            for (auto &key : keys) {
                auto objectId = helper->getObjectId(key);
                if (objectId > blockId ||
                    (objectId == blockId && blockOffset == 0)) {
                    keysToDelete.emplace_back(std::move(key));
                }
            }

            auto key = helper->getKey(fileId, blockId);
            auto blockSize = static_cast<std::size_t>(blockOffset);

            if (blockSize == 0 && blockId > 0) {
                key = helper->getKey(fileId, blockId - 1);
                blockSize = defBlockSize;
            }

            if (blockSize > 0 || blockId > 0) {
                Locks::accessor acc;
                locks->insert(acc, key);

                try {
                    auto buf = fillToSize(
                        readBlock(helper, key, 0, blockSize), blockSize);
                    helper->putObject(key, std::move(buf));
                    locks->erase(acc);
                }
                catch (...) {
                    locks->erase(acc);
                    LOG(ERROR) << "Truncate failed due to unknown error during "
                                  "'fillToSize'";
                    throw;
                }
            }

            if (!keysToDelete.empty())
                helper->deleteObjects(keysToDelete);
        }
        catch (const std::system_error &e) {
            logError("truncate", e);
            throw;
        }
    });
}

const Timeout &KeyValueAdapter::timeout()
{
    LOG_FCALL();

    return m_helper->timeout();
}

folly::Future<folly::IOBufQueue> KeyValueFileHandle::readBlocks(
    const off_t offset, const std::size_t requestedSize, const off_t fileSize)
{
    LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(requestedSize)
                << LOG_FARG(fileSize);

    const auto size =
        std::min(requestedSize, static_cast<std::size_t>(fileSize - offset));

    if (size == 0)
        return folly::makeFuture(
            folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()});

    folly::fbvector<folly::Future<folly::IOBufQueue>> readFutures;

    auto blockId = getBlockId(offset, m_blockSize);
    auto blockOffset = getBlockOffset(offset, m_blockSize);
    for (std::size_t bufOffset = 0; bufOffset < size;
         blockOffset = 0, ++blockId) {

        const auto blockSize =
            std::min(static_cast<std::size_t>(m_blockSize - blockOffset),
                static_cast<std::size_t>(size - bufOffset));

        auto readFuture = via(m_executor.get(), [
            this, blockId, blockOffset, blockSize, self = shared_from_this()
        ] {
            return fillToSize(
                readBlock(blockId, blockOffset, blockSize), blockSize);
        });

        readFutures.emplace_back(std::move(readFuture));
        bufOffset += blockSize;
    }

    return folly::collect(readFutures)
        .then([](std::vector<folly::IOBufQueue> &&results) {
            folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
            for (auto &subBuf : results)
                buf.append(std::move(subBuf));

            return buf;
        })
        .then([](folly::Try<folly::IOBufQueue> &&t) {
            try {
                return std::move(t.value());
            }
            catch (const std::system_error &e) {
                logError("read", e);
                throw;
            }
        });
}

folly::IOBufQueue KeyValueFileHandle::readBlock(
    const uint64_t blockId, const off_t blockOffset, const std::size_t size)
{
    LOG_FCALL() << LOG_FARG(blockId) << LOG_FARG(blockOffset) << LOG_FARG(size);

    auto key = m_helper->getKey(m_fileId, blockId);

    Locks::accessor acc;
    m_locks->insert(acc, key);

    try {
        auto ret = ::readBlock(m_helper, key, blockOffset, size);
        m_locks->erase(acc);
        return ret;
    }
    catch (...) {
        m_locks->erase(acc);
        throw;
    }
}

void KeyValueFileHandle::writeBlock(
    folly::IOBufQueue buf, const uint64_t blockId, const off_t blockOffset)
{
    LOG_FCALL() << LOG_FARG(buf.chainLength()) << LOG_FARG(blockId)
                << LOG_FARG(blockOffset);

    auto key = m_helper->getKey(m_fileId, blockId);
    Locks::accessor acc;
    m_locks->insert(acc, key);

    try {
        if (buf.chainLength() != m_blockSize) {
            auto fetchedBuf = ::readBlock(m_helper, key, 0, m_blockSize);

            folly::IOBufQueue filledBuf{folly::IOBufQueue::cacheChainLength()};

            if (blockOffset > 0) {
                if (!fetchedBuf.empty())
                    filledBuf.append(fetchedBuf.front()->clone());

                if (filledBuf.chainLength() >=
                    static_cast<std::size_t>(blockOffset))
                    filledBuf.trimEnd(filledBuf.chainLength() - blockOffset);

                filledBuf = fillToSize(std::move(filledBuf),
                    static_cast<std::size_t>(blockOffset));
            }

            filledBuf.append(std::move(buf));

            if (filledBuf.chainLength() < fetchedBuf.chainLength()) {
                fetchedBuf.trimStart(filledBuf.chainLength());
                filledBuf.append(std::move(fetchedBuf));
            }

            m_helper->putObject(key, std::move(filledBuf));
        }
        else {
            m_helper->putObject(key, std::move(buf));
        }

        m_locks->erase(acc);
    }
    catch (...) {
        m_locks->erase(acc);
        throw;
    }
}

folly::Future<FileHandlePtr> KeyValueAdapter::open(
    const folly::fbstring &fileId, const int /*flags*/,
    const Params &openParams)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARGM(openParams);

    FileHandlePtr handle = std::make_shared<KeyValueFileHandle>(
        fileId, m_helper, m_blockSize, m_locks, m_executor);

    return folly::makeFuture(std::move(handle));
}

} // namespace helpers
} // namespace one
