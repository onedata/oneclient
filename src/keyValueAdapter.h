/**
 * @file keyValueAdapter.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_KEY_VALUE_ADAPTER_H
#define HELPERS_KEY_VALUE_ADAPTER_H

#include "helpers/IStorageHelper.h"

#include <asio.hpp>
#include <tbb/concurrent_hash_map.h>

#include <memory>

namespace one {
namespace helpers {

constexpr std::size_t DEFAULT_BLOCK_SIZE = 5 * 1024 * 1024;

class KeyValueHelper;

/**
 * The @c KeyValueAdapter class translates POSIX operations to operations
 * available on key-value storage by splitting consistent range of bytes into
 * blocks.
 */
class KeyValueAdapter : public IStorageHelper {
public:
    using Locks = tbb::concurrent_hash_map<std::string, bool>;

    /**
     * Constructor.
     * @param helper @c KeyValueHelper instance that provides low level storage
     * access.
     * @param service IO service used for asynchronous operations.
     * @param locks Map of locks used to exclude concurrent operations on the
     * same storage block.
     * @param blockSize Size of storage block.
     */
    KeyValueAdapter(std::unique_ptr<KeyValueHelper> helper,
        asio::io_service &service, Locks &locks,
        std::size_t blockSize = DEFAULT_BLOCK_SIZE);

    CTXPtr createCTX(
        std::unordered_map<std::string, std::string> params) override;

    void ash_unlink(CTXPtr ctx, const boost::filesystem::path &p,
        VoidCallback callback) override;

    void ash_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        GeneralCallback<asio::mutable_buffer> callback) override;

    void ash_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset,
        GeneralCallback<std::size_t> callback) override;

    void ash_truncate(CTXPtr ctx, const boost::filesystem::path &p, off_t size,
        VoidCallback callback) override;

    void ash_mknod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        FlagsSet flags, dev_t rdev, VoidCallback callback) override
    {
        callback(SUCCESS_CODE);
    }

    void ash_mkdir(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback) override
    {
        callback(SUCCESS_CODE);
    }

    void ash_chmod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback) override
    {
        callback(SUCCESS_CODE);
    }

private:
    uint64_t getBlockId(off_t offset);
    off_t getBlockOffset(off_t offset);
    asio::mutable_buffer getBlock(
        CTXPtr ctx, std::string key, asio::mutable_buffer buf, off_t offset);
    void logError(std::string operation, const std::system_error &error);

    std::unique_ptr<KeyValueHelper> m_helper;
    asio::io_service &m_service;
    Locks &m_locks;
    std::size_t m_blockSize;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_KEY_VALUE_ADAPTER_H
