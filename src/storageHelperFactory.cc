/**
 * @file storageHelperFactory.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpers/storageHelperFactory.h"

#include "cephHelper.h"
#include "directIOHelper.h"
#include "keyValueAdapter.h"
#include "proxyIOHelper.h"
#include "s3Helper.h"

namespace one {
namespace helpers {

StorageHelperFactory::StorageHelperFactory(asio::io_service &cephService,
    asio::io_service &dioService, asio::io_service &kvService,
    std::shared_ptr<proxyio::BufferAgent> bufferAgent)
    : m_cephService{cephService}
    , m_dioService{dioService}
    , m_kvService{kvService}
    , m_bufferAgent{std::move(bufferAgent)}
{
}

std::shared_ptr<IStorageHelper> StorageHelperFactory::getStorageHelper(
    const std::string &sh_name,
    const std::unordered_map<std::string, std::string> &args)
{
    if (sh_name == CEPH_HELPER_NAME)
        return std::make_shared<CephHelper>(args, m_cephService);

    if (sh_name == DIRECT_IO_HELPER_NAME) {
#ifdef __linux__
        auto userCTXFactory = DirectIOHelper::linuxUserCTXFactory;
#else
        auto userCTXFactory = DirectIOHelper::noopUserCTXFactory;
#endif
        return std::make_shared<DirectIOHelper>(
            args, m_dioService, userCTXFactory);
    }

    if (sh_name == PROXY_IO_HELPER_NAME)
        return std::make_shared<ProxyIOHelper>(args, *m_bufferAgent);

    if (sh_name == S3_HELPER_NAME)
        return std::make_shared<KeyValueAdapter>(
            std::make_unique<S3Helper>(args), m_kvService, m_kvLocks);

    throw std::system_error{std::make_error_code(std::errc::invalid_argument),
        "Invalid storage helper name: '" + sh_name + "'"};
}

} // namespace helpers
} // namespace one
