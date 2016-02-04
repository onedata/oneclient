/**
 * @file helpersCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpersCache.h"

#include "communication/subscriptionData.h"
#include "messages/fuse/helperParams.h"
#include "messages/fuse/getHelperParams.h"
#include "messages/fuse/storageTestFile.h"
#include "messages/fuse/storageTestFileVerification.h"
#include "messages/status.h"

#include "messages.pb.h"

#include <boost/optional/optional_io.hpp>

#include <functional>

namespace one {
namespace client {

HelpersCache::HelpersCache(communication::Communicator &communicator)
    : m_communicator{communicator}
    , m_storageAccessManager{communicator, m_helperFactory}
{
    m_thread = std::thread{[this] { m_ioService.run(); }};

    auto predicate = [](const clproto::ServerMessage &message, const bool) {
        if (message.has_fuse_response()) {
            const auto &response = message.fuse_response();
            return response.has_storage_test_file() ||
                response.has_storage_test_file_verification();
        }
        return false;
    };
    auto callback = [this](const clproto::ServerMessage &message) {
        const auto &response = message.fuse_response();
        if (response.has_storage_test_file())
            handle(messages::Status{response.status()},
                messages::fuse::StorageTestFile{response.storage_test_file()});
        if (response.has_storage_test_file_verification())
            handle(messages::Status{response.status()},
                messages::fuse::StorageTestFileVerification{
                    response.storage_test_file_verification()});
    };
    m_communicator.subscribe(communication::SubscriptionData{
        std::move(predicate), std::move(callback)});
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
            m_storageAccessManager.createStorageTestFile(fileUuid, storageId);
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
    auto hashCombine = [](auto &seed, const auto &val) {
        std::hash<typename std::remove_const<
            typename std::remove_reference<decltype(val)>::type>::type> hasher;

        seed ^= hasher(val) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    };

    std::size_t hash = 0;
    hashCombine(hash, std::get<0>(k));
    hashCombine(hash, std::get<1>(k));
    return hash;
}

void HelpersCache::handle(const messages::Status &status,
    const messages::fuse::StorageTestFile &testFile)
{
    if (!status.code()) {
        auto helper = m_storageAccessManager.verifyStorageTestFile(testFile);

        if (helper) {
            CacheAccessor acc;
            if (m_cache.insert(
                    acc, std::make_tuple(testFile.storageId(), false)))
                acc->second = helper;
        }
        else {
            AccessTypeAccessor acc;
            if (m_accessType.insert(acc, testFile.storageId()))
                acc->second = AccessType::PROXY;
        }
    }
    else {
        LOG(ERROR) << "Unknown storage test file creation error, code: '"
                   << status.code() << "', description: '"
                   << status.description() << "'";
    }
}

void HelpersCache::handle(const messages::Status &status,
    const messages::fuse::StorageTestFileVerification &testFileVerification)
{
    AccessTypeAccessor acc;
    if (!status.code()) {
        if (m_accessType.insert(acc, testFileVerification.storageId())) {
            LOG(INFO) << "Storage '" << testFileVerification.storageId()
                      << "' is directly accessible to the client.";
            acc->second = AccessType::DIRECT;
        }
    }
    else if (status.code().value() == ENOENT ||
        status.code().value() == EINVAL) {
        if (m_accessType.insert(acc, testFileVerification.storageId())) {
            LOG(INFO) << "Storage '" << testFileVerification.storageId()
                      << "' is not directly accessible to the client.";
            acc->second = AccessType::PROXY;
        }
    }
    else {
        LOG(ERROR) << "Unknown storage test file verification error, code: '"
                   << status.code() << "', description: '"
                   << status.description() << "'";
    }
}

} // namespace one
} // namespace client
