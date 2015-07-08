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

#include <memory>
#include <string>
#include <asio/io_service.hpp>

namespace one {

namespace communication {
class Communicator;
}

namespace helpers {
namespace utils {
std::string tolower(std::string input);
}

std::string srvArg(const int argno);

struct BufferLimits {
    BufferLimits(const size_t wgl = 0, const size_t rgl = 0,
        const size_t wfl = 0, const size_t rfl = 0,
        const size_t pbs = 4 * 1024);

    const size_t writeBufferGlobalSizeLimit;
    const size_t readBufferGlobalSizeLimit;

    const size_t writeBufferPerFileSizeLimit;
    const size_t readBufferPerFileSizeLimit;

    const size_t preferedBlockSize;
};

/**
 * Factory providing objects of requested storage helpers.
 */
class StorageHelperFactory {
public:
    StorageHelperFactory() = default;
    StorageHelperFactory(
        std::shared_ptr<communication::Communicator> communicator,
        const BufferLimits &limits, asio::io_service &dio_service,
        asio::io_service &cproxy_service);
    virtual ~StorageHelperFactory() = default;

    /**
     * Produces storage helper object.
     * @param sh Name of storage helper that has to be returned.
     * @param args Arguments map passed as argument to storge helper's
     * constructor.
     * @return Pointer to storage helper object along with its ownership.
     */
    virtual std::shared_ptr<IStorageHelper> getStorageHelper(
        const std::string &sh, const IStorageHelper::ArgsMap &args);

private:
    const std::shared_ptr<communication::Communicator> m_communicator;
    const BufferLimits m_limits;
    asio::io_service &m_dio_service;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_STORAGE_HELPER_FACTORY_H
