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
#include "helpers/IStorageHelper.h"
#include "helpers/storageHelperFactory.h"
#include "messages/fuse/storageTestFile.h"

#include <boost/filesystem.hpp>

#include <vector>

namespace one {
namespace client {

/**
 * The StorageAccessManager class is responsible for detecting storages that are
 * directly accessible to the client.
 */
class StorageAccessManager {
public:
    /**
     * Constructor.
     * @param communicator Reference to the @c communication::Communicator
     * instance.
     * @param helperFactory Reference to the @c helpers::StorageHelperFactory
     * instance.
     */
    StorageAccessManager(communication::Communicator &communicator,
        helpers::StorageHelperFactory &helperFactory);

    /**
     * Sends storage test file creation request to the server.
     * @param fileUuid UUID of a file for which storage helper has been
     * retrieved, in the directory of this file an actual storage test file will
     * be created.
     * @param storageId ID of a storage on which the test file will be created.
     */
    void createStorageTestFile(
        const std::string &fileUuid, const std::string &storageId);

    /**
     * Verifies the test file by reading it from the storage and checking its
     * content with the one sent by the server. Next modifies the test file and
     * request modification verification by the server.
     * @param testFile Reference to the @c messages::fuse::StorageTestFile
     * instance.
     * @return In case of a successful verification a pointer to storage helper
     * object used to access the test file, otherwise @c nullptr.
     */
    std::shared_ptr<helpers::IStorageHelper> verifyStorageTestFile(
        const messages::fuse::StorageTestFile &testFile);

private:
    bool verifyStorageTestFile(std::shared_ptr<helpers::IStorageHelper> helper,
        const messages::fuse::StorageTestFile &testFile);

    std::vector<boost::filesystem::path> getMountPoints() const;

    communication::Communicator &m_communicator;
    helpers::StorageHelperFactory &m_helperFactory;
    std::vector<boost::filesystem::path> m_mountPoints;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_STORAGE_ACCESS_MANAGER_H
