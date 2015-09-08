#ifndef ONECLIENT_HELPER_WRAPPER_H
#define ONECLIENT_HELPER_WRAPPER_H

#include "helpers/IStorageHelper.h"

namespace one {
namespace client {

class HelperWrapper {
public:
    HelperWrapper(helpers::IStorageHelper &helper);
    HelperWrapper(
        helpers::IStorageHelper &helper, helpers::StorageHelperCTX &context);

    void mknod(const boost::filesystem::path &p, mode_t mode, dev_t rdev);

private:
    helpers::StorageHelperCTX m_defaultContext;

    helpers::IStorageHelper &m_helper;
    helpers::StorageHelperCTX &m_context;
};

} // namespace one
} // namespace client

#endif // ONECLIENT_HELPER_WRAPPER_H
