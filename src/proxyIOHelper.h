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

#include "communication/communicator.h"
#include "proxyio/bufferAgent.h"

#include <string>

namespace one {
namespace helpers {

struct ProxyIOHelperCTX : public IStorageHelperCTX {
};

class ProxyIOHelper : public IStorageHelper {
public:
    ProxyIOHelper(const std::unordered_map<std::string, std::string> &args,
        proxyio::BufferAgent &bufferAgent);

    CTXPtr createCTX() override;

    int sh_open(CTXPtr ctx, const boost::filesystem::path &p, int flags,
        const std::string &fileUuid) override;

    asio::mutable_buffer sh_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset) override;

    std::size_t sh_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset) override;

    void sh_flush(CTXPtr ctx, const boost::filesystem::path &p) override;

    void sh_fsync(
        CTXPtr ctx, const boost::filesystem::path &p, bool isDataSync) override;

    void sh_release(CTXPtr ctx, const boost::filesystem::path &p) override;

private:
    std::string m_storageId;
    proxyio::BufferAgent &m_bufferAgent;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_PROXYIO_PROXYIO_HELPER_H
