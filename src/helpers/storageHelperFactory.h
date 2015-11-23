/**
 * @file storageHelperFactory.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_STORAGE_HELPER_FACTORY_H
#define HELPERS_STORAGE_HELPER_FACTORY_H

#include "helpers/IStorageHelper.h"

#include "communication/communicator.h"

#include <asio/io_service.hpp>

#include <memory>
#include <string>

namespace one {
namespace helpers {

/**
 * Factory providing objects of requested storage helpers.
 */
class StorageHelperFactory {
public:
    StorageHelperFactory(asio::io_service &dio_service,
        communication::Communicator &communicator);

    virtual ~StorageHelperFactory() = default;

    /**
     * Produces storage helper object.
     * @param sh Name of storage helper that has to be returned.
     * @param args Arguments map passed as argument to storge helper's
     * constructor.
     * @return Pointer to storage helper object along with its ownership.
     */
    virtual std::shared_ptr<IStorageHelper> getStorageHelper(
        const std::string &sh,
        const std::unordered_map<std::string, std::string> &args);

private:
    asio::io_service &m_dioService;
    communication::Communicator &m_communicator;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_STORAGE_HELPER_FACTORY_H
