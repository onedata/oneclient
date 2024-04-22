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
#include "helpers/storageHelperCreator.h"
#include "messages/fuse/createStorageTestFile.h"
#include "messages/fuse/storageTestFile.h"
#include "messages/fuse/verifyStorageTestFile.h"
#include "options/options.h"
#include "posixHelper.h"

#include <boost/filesystem.hpp>
#include <folly/FBString.h>

#include <vector>

namespace one {
namespace helpers {
class StorageHelper;
// class StorageHelperCreator;
}
namespace messages {
namespace fuse {
class StorageTestFile;
} // namespace fuse
} // namespace messages

namespace client {

/**
 * Returns true if overrideParams contains 'mountPoint' parameter
 * and it is a subdirectory of an existing mountpoint on the host.
 *
 * @param storageId ID of the storage for which the helper is being created.
 * @param overrideParams The dictionary of override params for the helper.
 * @return True if mountPoint exists and is valid.
 */
bool checkPosixMountpointOverride(const folly::fbstring &storageId,
    const std::unordered_map<folly::fbstring, folly::fbstring> &overrideParams);

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

namespace detail {
std::vector<boost::filesystem::path> getMountPoints();

bool verifyStorageTestFile(const folly::fbstring &storageId,
    std::shared_ptr<helpers::StorageHelper> helper,
    const messages::fuse::StorageTestFile &testFile);
}
/**
 * The StorageAccessManager class is responsible for detecting storages that are
 * directly accessible to the client.
 */
template <typename CommunicatorT> class StorageAccessManager {
public:
    /**
     * Constructor.
     * @param helperFactory Instance of @c helpers::StorageHelperCreator.
     */
    StorageAccessManager(
        helpers::StorageHelperCreator<CommunicatorT> &helperFactory,
        const options::Options &options)
        : m_helperFactory{helperFactory}
        , m_options{options}
    {
    }

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
        const messages::fuse::StorageTestFile &testFile)
    {
        const auto &helperParams = testFile.helperParams();
        const auto &overrideParams =
            m_options.getHelperOverrideParams(storageId);

        if (helperParams.name() == helpers::POSIX_HELPER_NAME) {
            std::vector<boost::filesystem::path> mountPoints;

            // Check if the mount point is provided during integration tests
            if (helperParams.args().find("testMountPoint") !=
                helperParams.args().cend()) {
                mountPoints.emplace_back(
                    helperParams.args().at("testMountPoint").toStdString());
            }
            else {
                // List all mountpoints in the system for automatic detection
                mountPoints = detail::getMountPoints();
            }

            for (const auto &mountPoint : mountPoints) {
                LOG_DBG(2) << "Verifying POSIX storage " << storageId
                           << " test file under mountpoint " << mountPoint;

                auto helper =
                    m_helperFactory.getStorageHelper(helpers::POSIX_HELPER_NAME,
                        {{helpers::POSIX_HELPER_MOUNT_POINT_ARG,
                            mountPoint.string()}},
                        m_options.isIOBuffered());

                if (!helper) {
                    continue;
                }

                try {
                    if (detail::verifyStorageTestFile(
                            storageId, helper, testFile)) {
                        LOG_DBG(2)
                            << "POSIX storage " << storageId
                            << " successfully located under " << mountPoint;
                        return helper;
                    }
                }
                catch (const std::exception &e) {
                    LOG_DBG(2)
                        << "Verification of POSIX storage under mountpoint "
                        << mountPoint << " failed due to: " << e.what();
                }
            }
        }
        else if ((helperParams.name() == helpers::NULL_DEVICE_HELPER_NAME)
#if WITH_WEBDAV
            || (helperParams.name() == helpers::HTTP_HELPER_NAME)
#endif
        ) {
            return m_helperFactory.getStorageHelper(helperParams.name(),
                helperParams.args(), m_options.isIOBuffered(), overrideParams);
        }
        else {
            auto helper = m_helperFactory.getStorageHelper(helperParams.name(),
                helperParams.args(), m_options.isIOBuffered(), overrideParams);

            bool skipStorageDetection = false;

            if (helperParams.args().find("skipStorageDetection") !=
                    helperParams.args().cend() &&
                helperParams.args().at("skipStorageDetection") == "true")
                skipStorageDetection = true;

            // Command line override has higher priority than server settings
            if (overrideParams.find("skipStorageDetection") !=
                overrideParams.cend()) {
                if (overrideParams.at("skipStorageDetection") == "true")
                    skipStorageDetection = true;
                else if (overrideParams.at("skipStorageDetection") == "false")
                    skipStorageDetection = false;
                else
                    LOG(WARNING) << "Invalid value "
                                 << overrideParams.at("skipStorageDetection")
                                 << " provided for skipStorageDetection";
            }

            if (skipStorageDetection)
                return helper;

            if (detail::verifyStorageTestFile(storageId, helper, testFile)) {
                LOG(INFO) << helperParams.name() << " storage " << storageId
                          << " successfully detected";
                return helper;
            }
        }

        return {};
    }

private:
    helpers::StorageHelperCreator<CommunicatorT> &m_helperFactory;

    // Reference to command line options provided to Oneclient
    const options::Options &m_options;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_STORAGE_ACCESS_MANAGER_H
