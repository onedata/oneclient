/**
 * @file helpersCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpersCache.h"

#include "messages.pb.h"
#include "messages/fuse/createStorageTestFile.h"
#include "messages/fuse/getHelperParams.h"
#include "messages/fuse/helperParams.h"
#include "messages/fuse/storageTestFile.h"
#include "messages/fuse/verifyStorageTestFile.h"

#include <folly/ThreadName.h>

#include <algorithm>
#include <chrono>
#include <functional>

namespace one {
namespace client {
namespace cache {

constexpr unsigned int VERIFY_TEST_FILE_ATTEMPTS = 5;
constexpr std::chrono::seconds VERIFY_TEST_FILE_DELAY{15};

HelpersCache::HelpersCache(communication::Communicator &communicator,
    Scheduler &scheduler, const std::size_t workersNumber)
    : m_communicator{communicator}
    , m_scheduler{scheduler}
    , m_helpersIoService{workersNumber}
{
    std::generate_n(
        std::back_inserter(m_helpersWorkers), workersNumber, [this] {
            return std::thread{[this] {
                folly::setThreadName("HelpersWorker");
                m_helpersIoService.run();
            }};
        });
}

HelpersCache::~HelpersCache()
{
    m_helpersIoService.stop();
    for (auto &worker : m_helpersWorkers)
        worker.join();
}

HelpersCache::HelperPtr HelpersCache::get(const folly::fbstring &fileUuid,
    const folly::fbstring &storageId, const bool forceProxyIO)
{
    if (!forceProxyIO) {
        decltype(m_accessType)::iterator accessTypeIt;
        bool accessUnset;
        std::tie(accessTypeIt, accessUnset) =
            m_accessType.emplace(std::make_pair(storageId, AccessType::PROXY));

        if (accessUnset) {
            accessTypeIt->second = AccessType::PROXY;
            requestStorageTestFileCreation(fileUuid, storageId);
            return get(fileUuid, storageId, true);
        }

        if (accessTypeIt->second == AccessType::PROXY)
            return get(fileUuid, storageId, true);
    }

    auto key = std::make_pair(storageId, forceProxyIO);

    auto helperIt = m_cache.find(key);
    if (helperIt != m_cache.end())
        return helperIt->second;

    // TODO: multiple requests may go out before helper is resolved
    auto params = communication::wait(
        m_communicator.communicate<messages::fuse::HelperParams>(
            messages::fuse::GetHelperParams{
                storageId.toStdString(), forceProxyIO}));

    auto helper = m_helperFactory.getStorageHelper(
        params.name(), params.args(), /*buffered*/ true);

    m_cache[key] = helper;
    return helper;
}

void HelpersCache::requestStorageTestFileCreation(
    const folly::fbstring &fileUuid, const folly::fbstring &storageId)
{
    DLOG(INFO) << "Requesting storage test file creation for file: '"
               << fileUuid << "' and storage: '" << storageId << "'";

    try {
        auto testFile = communication::wait(
            m_communicator.communicate<messages::fuse::StorageTestFile>(
                messages::fuse::CreateStorageTestFile{
                    fileUuid.toStdString(), storageId.toStdString()}));

        auto sharedTestFileMsg =
            std::make_shared<messages::fuse::StorageTestFile>(
                std::move(testFile));

        handleStorageTestFile(
            sharedTestFileMsg, storageId, VERIFY_TEST_FILE_ATTEMPTS);
    }
    catch (const std::system_error &e) {
        LOG(WARNING) << "Storage test file creation error, code: '" << e.code()
                     << "', message: '" << e.what() << "'";

        if (e.code().value() == EAGAIN)
            m_accessType.erase(storageId);
        else
            LOG(INFO) << "Storage '" << storageId
                      << "' is not directly accessible to the client.";
    }
}

void HelpersCache::handleStorageTestFile(
    std::shared_ptr<messages::fuse::StorageTestFile> testFile,
    const folly::fbstring &storageId, const std::size_t attempts)
{
    DLOG(INFO) << "Handling storage test file: " << testFile->toString()
               << " for storage: '" << storageId
               << "' with left attempts: " << attempts << ".";

    if (attempts == 0) {
        LOG(INFO) << "Storage '" << storageId
                  << "' is not directly accessible to the client. Test "
                     "file verification attempts limit exceeded.";

        m_accessType[storageId] = AccessType::PROXY;
        return;
    }

    try {
        auto helper = m_storageAccessManager.verifyStorageTestFile(*testFile);
        if (!helper) {
            m_scheduler.schedule(
                VERIFY_TEST_FILE_DELAY, [ =, testFile = std::move(testFile) ] {
                    handleStorageTestFile(testFile, storageId, attempts - 1);
                });
            return;
        }

        auto fileContent =
            m_storageAccessManager.modifyStorageTestFile(helper, *testFile);
        requestStorageTestFileVerification(*testFile, storageId, fileContent);

        m_cache[std::make_pair(storageId, false)] = helper;
    }
    catch (const std::system_error &e) {
        LOG(ERROR) << "Storage test file handling error, code: '" << e.code()
                   << "', message: '" << e.what() << "'";

        if (e.code().value() == EAGAIN) {
            m_accessType.erase(storageId);
        }
        else {
            LOG(INFO) << "Storage '" << storageId
                      << "' is not directly accessible to the client.";

            m_accessType[storageId] = AccessType::PROXY;
        }
    }
}

void HelpersCache::requestStorageTestFileVerification(
    const messages::fuse::StorageTestFile &testFile,
    const folly::fbstring &storageId, const folly::fbstring &fileContent)
{
    DLOG(INFO) << "Requesting verification of storage: '" << storageId
               << "' with file: '" << testFile.toString()
               << "' and modified content '" << fileContent << ".";

    messages::fuse::VerifyStorageTestFile request{storageId.toStdString(),
        testFile.spaceId(), testFile.fileId(), fileContent.toStdString()};

    try {
        communication::wait(
            m_communicator.communicate<messages::fuse::FuseResponse>(
                std::move(request)));

        handleStorageTestFileVerification({}, storageId);
    }
    catch (const std::system_error &e) {
        handleStorageTestFileVerification(e.code(), {});
    }
}

void HelpersCache::handleStorageTestFileVerification(
    const std::error_code &ec, const folly::fbstring &storageId)
{
    DLOG(INFO) << "Handling verification of storage: '" << storageId << "'.";

    if (!ec) {
        LOG(INFO) << "Storage '" << storageId
                  << "' is directly accessible to the client.";

        m_accessType[storageId] = AccessType::DIRECT;
    }
    else {
        LOG(ERROR) << "Storage test file verification error, code: '"
                   << ec.value() << "', message: '" << ec.message() << "'";

        if (ec.value() == EAGAIN) {
            m_accessType.erase(storageId);
        }
        else {
            LOG(INFO) << "Storage '" << storageId
                      << "' is not directly accessible to the client.";

            m_accessType[storageId] = AccessType::PROXY;
        }
    }
}

} // namespace cache
} // namespace client
} // namespace one
