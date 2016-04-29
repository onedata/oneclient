/**
 * @file proxyIOHelper.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_PROXY_IO_HELPER_H
#define HELPERS_PROXY_IO_HELPER_H

#include "helpers/IStorageHelper.h"

#include <asio/io_service.hpp>

#include <cstdint>

namespace one {
namespace helpers {

namespace proxyio {
class BufferAgent;
} // namespace proxyio

class ProxyIOHelper : public IStorageHelper {
public:
    ProxyIOHelper(const std::unordered_map<std::string, std::string> &args,
        proxyio::BufferAgent &bufferAgent);

    int sh_open(
        CTXPtr ctx, const boost::filesystem::path &p, int flags) override;

    asio::mutable_buffer sh_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset) override;

    std::size_t sh_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset) override;

    void sh_flush(CTXPtr ctx, const boost::filesystem::path &p) override;

    void sh_fsync(
        CTXPtr ctx, const boost::filesystem::path &p, bool isDataSync) override;

    void sh_release(CTXPtr ctx, const boost::filesystem::path &p) override;

private:
    proxyio::BufferAgent &m_bufferAgent;
    std::string m_storageId;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_PROXY_IO_HELPER_H
