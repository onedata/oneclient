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
#include "utils.hpp"

#include <boost/functional/hash.hpp>
#include <boost/optional/optional_io.hpp>

#include <chrono>
#include <functional>

namespace one {
namespace client {

constexpr unsigned int VERIFY_TEST_FILE_ATTEMPTS = 5;
constexpr std::chrono::seconds VERIFY_TEST_FILE_DELAY{15};

HelpersCache::HelpersCache(
    communication::Communicator &communicator, Scheduler &scheduler)
    : m_communicator{communicator}
    , m_scheduler{scheduler}
    , m_storageAccessManager{communicator, m_helperFactory}
{
    m_thread = std::thread{[this] {
        etls::utils::nameThread("HelpersCache");
        m_ioService.run();
    }};
}

HelpersCache::~HelpersCache()
{
    m_ioService.stop();
    m_thread.join();
}

HelpersCache::HelperPtr HelpersCache::get(const std::string &fileUuid,
    const std::string &storageId, bool forceProxyIO)
{
    if (!forceProxyIO) {
        ConstAccessTypeAccessor constAcc;
        if (m_accessType.find(constAcc, storageId)) {
            forceProxyIO = (constAcc->second == AccessType::PROXY);
        }
        else {
            AccessTypeAccessor acc;
            if (m_accessType.insert(acc, storageId))
                acc->second = AccessType::PROXY;
            acc.release();
            requestStorageTestFileCreation(fileUuid, storageId);
            forceProxyIO = true;
        }
    }

    ConstCacheAccessor constAcc;
    if (m_cache.find(constAcc, std::make_tuple(storageId, forceProxyIO)))
        return constAcc->second;

    CacheAccessor acc;
    if (!m_cache.insert(acc, std::make_tuple(storageId, forceProxyIO)))
        return acc->second;

    try {
        auto future = m_communicator.communicate<messages::fuse::HelperParams>(
            messages::fuse::GetHelperParams(storageId, forceProxyIO));

        auto params = communication::wait(future);
        auto helper =
            m_helperFactory.getStorageHelper(params.name(), params.args());

        acc->second = helper;
        return helper;
    }
    catch (...) {
        m_cache.erase(acc);
        throw;
    }
}

bool HelpersCache::HashCompare::equal(const std::tuple<std::string, bool> &j,
    const std::tuple<std::string, bool> &k) const
{
    return j == k;
}

size_t HelpersCache::HashCompare::hash(
    const std::tuple<std::string, bool> &k) const
{
    return boost::hash<std::tuple<std::string, bool>>{}(k);
}

void HelpersCache::requestStorageTestFileCreation(
    const std::string &fileUuid, const std::string &storageId)
{
    messages::fuse::CreateStorageTestFile request{fileUuid, storageId};

    m_communicator.communicate<messages::fuse::StorageTestFile>(
        std::move(request),
        [=](const std::error_code &ec,
            std::unique_ptr<messages::fuse::StorageTestFile> testFile) {
            if (!ec) {
                handleStorageTestFile(
                    std::make_shared<messages::fuse::StorageTestFile>(
                        std::move(*testFile)),
                    storageId, VERIFY_TEST_FILE_ATTEMPTS);
            }
            else {
                LOG(ERROR)
                    << "Unknown storage test file creation error, code: '"
                    << ec.value() << "', message: '" << ec.message() << "'";
            }
        });
}

void HelpersCache::handleStorageTestFile(
    std::shared_ptr<messages::fuse::StorageTestFile> testFile,
    const std::string &storageId, unsigned int attempts)
{
    if (attempts == 0) {
        AccessTypeAccessor acc;
        if (m_accessType.insert(acc, storageId))
            acc->second = AccessType::PROXY;
        return;
    }

    try {
        auto helper = m_storageAccessManager.verifyStorageTestFile(*testFile);

        if (helper == nullptr) {
            m_scheduler.schedule(
                VERIFY_TEST_FILE_DELAY, [ =, testFile = std::move(testFile) ] {
                    handleStorageTestFile(testFile, storageId, attempts - 1);
                });
            return;
        }

        auto fileContent =
            m_storageAccessManager.modifyStorageTestFile(helper, *testFile);
        requestStorageTestFileVerification(
            *testFile, storageId, std::move(fileContent));

        CacheAccessor acc;
        if (m_cache.insert(acc, std::make_tuple(storageId, false)))
            acc->second = helper;
    }
    catch (const std::system_error &e) {
        auto code = e.code().value();
        if (code == ENOENT || code == ENOTDIR || code == EPERM) {
            AccessTypeAccessor acc;
            if (m_accessType.insert(acc, storageId))
                acc->second = AccessType::PROXY;
        }
        else {
            LOG(ERROR) << "Unexpected error occurred, code: " << e.code()
                       << ", message: '" << e.what() << "'.";
        }
    }
}

void HelpersCache::requestStorageTestFileVerification(
    const messages::fuse::StorageTestFile &testFile,
    const std::string &storageId, std::string fileContent)
{
    messages::fuse::VerifyStorageTestFile request{storageId,
        testFile.spaceUuid(), testFile.fileId(), std::move(fileContent)};

    m_communicator.communicate<messages::fuse::FuseResponse>(
        std::move(request),
        [=](const std::error_code &ec,
            std::unique_ptr<messages::fuse::FuseResponse> response) {
            handleStorageTestFileVerification(ec, storageId);
        });
}

void HelpersCache::handleStorageTestFileVerification(
    const std::error_code &ec, const std::string &storageId)
{
    AccessTypeAccessor acc;
    m_accessType.insert(acc, storageId);
    if (!ec) {
        LOG(INFO) << "Storage '" << storageId
                  << "' is directly accessible to the client.";
        acc->second = AccessType::DIRECT;
    }
    else if (ec.value() == ENOENT || ec.value() == EINVAL) {
        LOG(INFO) << "Storage '" << storageId
                  << "' is not directly accessible to the client.";
        acc->second = AccessType::PROXY;
    }
    else {
        LOG(ERROR) << "Unknown storage test file verification error, code: '"
                   << ec.value() << "', message: '" << ec.message() << "'";
    }
}

} // namespace one
} // namespace client
