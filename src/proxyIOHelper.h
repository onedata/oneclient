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

#include <asio/io_service.hpp>

#include <cstdint>

namespace one {
namespace helpers {

class ProxyIOHelperCTX : public IStorageHelperCTX {
public:
    void setUserCTX(std::unordered_map<std::string, std::string> args) {}

    void setFlags(int flags) {}
};

class ProxyIOHelper : public IStorageHelper {
public:
    ProxyIOHelper(const std::unordered_map<std::string, std::string> &args,
        communication::Communicator &communicator);

    CTXPtr createCTX();

    void ash_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        GeneralCallback<asio::mutable_buffer>);

    void ash_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t>);

private:
    communication::Communicator &m_communicator;
    std::string m_storageId;
    std::string m_spaceId;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_PROXY_IO_HELPER_H
