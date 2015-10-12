/**
 * @file helperWrapper.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_HELPER_WRAPPER_H
#define ONECLIENT_HELPER_WRAPPER_H

#include "helpers/IStorageHelper.h"

namespace one {
namespace client {

/**
 * @c HelperWrapper serves to temporarily wrap a @c helpers::IStorageHelper
 * instance in order to transform its operations from asynchronous to
 * synchronous.
 */
class HelperWrapper {
public:
    /**
     * Constructor
     * @param helper A @c helpers::IStorageHelper instance to wrap.
     */
    HelperWrapper(helpers::IStorageHelper &helper);

    /**
     * Constructor.
     * @param helper A @c helpers::IStorageHelper instance to wrap.
     * @param context A @c helpers::StorageHelperCTX instance to use for ops.
     */
    HelperWrapper(
        helpers::IStorageHelper &helper, helpers::StorageHelperCTX &context);

    /**@{*/
    /**
     * Wrapped @c helpers::IStorageHelper operations.
     * Refer to @c helpers::IStorageHelper documentation for more information.
     */
    asio::mutable_buffer read(const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset);

    std::size_t write(
        const boost::filesystem::path &p, asio::const_buffer buf, off_t offset);
    /**@}*/

private:
    helpers::StorageHelperCTX m_defaultContext;

    helpers::IStorageHelper &m_helper;
    helpers::StorageHelperCTX &m_context;
};

} // namespace one
} // namespace client

#endif // ONECLIENT_HELPER_WRAPPER_H
