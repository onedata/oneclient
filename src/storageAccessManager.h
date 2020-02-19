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
     * @param storageId Id of the storage
     * @param testFile Instance of @c messages::fuse::StorageTestFile.
     * @return Storage helper object used to access the test file or nullptr if
     * verification fails.
     */
    std::shared_ptr<helpers::StorageHelper> verifyStorageTestFile(
        const folly::fbstring &storageId,
        const messages::fuse::StorageTestFile &testFile);

    /**
     * Modifies the test file by writing random sequence of characters.
     * @param storageId Id of the storage
     * @param helper Storage helper object used to access the test file.
     * @param testFile Instance of @c messages::fuse::StorageTestFile.
     * @return Modified content of the test file.
     */
    folly::fbstring modifyStorageTestFile(const folly::fbstring &storageId,
        std::shared_ptr<helpers::StorageHelper> helper,
        const messages::fuse::StorageTestFile &testFile);

    /**
     * Returns true if overrideParams contains 'mountPoint' parameter
     * and it is a subdirectory of an existing mountpoint on the host.
     *
     * @param storageId ID of the storage for which the helper is being created.
     * @param overrideParams The dictionary of override params for the helper.
     * @return True if mountPoint exists and is valid.
     */
    bool checkPosixMountpointOverride(const folly::fbstring &storageId,
        const std::unordered_map<folly::fbstring, folly::fbstring>
            &overrideParams);

private:
    bool verifyStorageTestFile(const folly::fbstring &storageId,
        std::shared_ptr<helpers::StorageHelper> helper,
        const messages::fuse::StorageTestFile &testFile);

    helpers::StorageHelperCreator &m_helperFactory;

    // Reference to command line options provided to Oneclient
    const options::Options &m_options;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_STORAGE_ACCESS_MANAGER_H
