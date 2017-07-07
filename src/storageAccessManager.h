/**
 * @file storageAccessManager.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_STORAGE_ACCESS_MANAGER_H
#define ONECLIENT_STORAGE_ACCESS_MANAGER_H

#include "communication/communicator.h"
#include "options/options.h"

#include <boost/filesystem.hpp>
#include <folly/FBString.h>

#include <vector>

namespace one {
namespace helpers {
class StorageHelper;
class StorageHelperCreator;
}
namespace messages {
namespace fuse {
class StorageTestFile;
}
}
namespace client {

/**
 * The StorageAccessManager class is responsible for detecting storages that are
 * directly accessible to the client.
 */
class StorageAccessManager {
public:
    /**
     * Constructor.
     * @param helperFactory Instance of @c helpers::StorageHelperCreator.
     */
    StorageAccessManager(helpers::StorageHelperCreator &helperFactory,
        const options::Options &options);

    /**
     * Verifies the test file by reading it from the storage and checking its
     * content with the one sent by the server.
     * @param testFile Instance of @c messages::fuse::StorageTestFile.
     * @return Storage helper object used to access the test file or nullptr if
     * verification fails.
     */
    std::shared_ptr<helpers::StorageHelper> verifyStorageTestFile(
        const messages::fuse::StorageTestFile &testFile);

    /**
     * Modifies the test file by writing random sequence of characters.
     * @param helper Storage helper object used to access the test file.
     * @param testFile Instance of @c messages::fuse::StorageTestFile.
     * @return Modified content of the test file.
     */
    folly::fbstring modifyStorageTestFile(
        std::shared_ptr<helpers::StorageHelper> helper,
        const messages::fuse::StorageTestFile &testFile);

private:
    bool verifyStorageTestFile(std::shared_ptr<helpers::StorageHelper> helper,
        const messages::fuse::StorageTestFile &testFile);

    helpers::StorageHelperCreator &m_helperFactory;
    const options::Options &m_options;
    std::vector<boost::filesystem::path> m_mountPoints;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_STORAGE_ACCESS_MANAGER_H
