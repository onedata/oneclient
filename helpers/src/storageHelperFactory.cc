/**
 * @file storageHelperFactory.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpers/storageHelperFactory.h"

#include "directIOHelper.h"
#include "proxyIOHelper.h"

namespace one {
namespace helpers {

StorageHelperFactory::StorageHelperFactory(
    asio::io_service &dio_service, communication::Communicator &communicator)
    : m_dioService{dio_service}
    , m_communicator{communicator}
{
}

std::shared_ptr<IStorageHelper> StorageHelperFactory::getStorageHelper(
    const std::string &sh_name,
    const std::unordered_map<std::string, std::string> &args)
{
    if (sh_name == "DirectIO")
        return std::make_shared<DirectIOHelper>(args, m_dioService);
    if (sh_name == "ProxyIO")
        return std::make_shared<ProxyIOHelper>(args, m_communicator);

    return {};
}

} // namespace helpers
} // namespace one
