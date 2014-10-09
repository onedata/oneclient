/**
 * @file storageMapper.cc
 * @author Beata Skiba
 * @author Rafal Slota
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "storageMapper.h"

#include "communication_protocol.pb.h"
#include "context.h"
#include "fslogicProxy.h"
#include "fuse_messages.pb.h"
#include "helpers/storageHelperFactory.h"
#include "logging.h"
#include "oneException.h"
#include "fsImpl.h"
#include "scheduler.h"

#include <boost/algorithm/string/predicate.hpp>
#include <boost/any.hpp>

#include <ctime>

namespace icl = boost::icl;
using namespace one::clproto::communication_protocol;
using namespace one::clproto::fuse_messages;

namespace one {
namespace client {

StorageMapper::StorageMapper(std::weak_ptr<Context> context,
                             std::shared_ptr<FslogicProxy> fslogicProxy)
    : m_fslogic(std::move(fslogicProxy))
    , m_context{std::move(context)}
{
}

std::pair<LocationInfo, StorageInfo>
StorageMapper::getLocationInfo(const std::string &logicalName, bool useCluster,
                               bool forceClusterProxy)
{
    auto locationInfo =
        retrieveLocationInfo(logicalName, useCluster, forceClusterProxy);
    auto storageInfo = retrieveStorageInfo(locationInfo);
    return {std::move(locationInfo), std::move(storageInfo)};
}

std::string StorageMapper::findLocation(const std::string &logicalName,
                                        const std::string &openMode,
                                        bool forceClusterProxy)
{
    LOG(INFO) << "quering cluster about storage mapping for file: "
              << logicalName;

    FileLocation location;
    if (m_fslogic->getFileLocation(logicalName, location, openMode,
                                   forceClusterProxy)) {
        if (location.answer() == VOK)
            addLocation(logicalName, location);
        else
            LOG(WARNING) << "Cannot get storage mapping for file: "
                         << logicalName
                         << " due to error: " << location.answer();

        return location.answer();
    }

    return VEIO;
}

void StorageMapper::addLocation(const std::string &logicalName,
                                const FileLocation &location)
{
    LOG(INFO) << "adding location for file '" << logicalName
              << "', fileId: " << location.file_id()
              << ", storageId: " << location.storage_id()
              << ", validTo: " << time(nullptr) << " + " << location.validity();

    addFileMapping(logicalName, location);
    addStorageMapping(location);
}

void StorageMapper::openFile(const std::string &logicalName)
{
    LOG(INFO) << "marking file '" << logicalName << "' as open";

    boost::lock_guard<boost::shared_mutex> guard{m_fileMappingMutex};
    const auto it = mappingFromLogicalName(logicalName);
    if (it != m_fileMapping.end())
        ++it->second.opened;
}

void StorageMapper::releaseFile(const std::string &logicalName)
{
    LOG(INFO) << "marking file '" << logicalName << "' closed";

    boost::unique_lock<boost::shared_mutex> lock{m_fileMappingMutex};
    const auto it = mappingFromLogicalName(logicalName);

    // The file was not opened
    if (it == m_fileMapping.end() || it->second.opened == 0)
        return;

    if (--it->second.opened == 0) {
        m_fileMapping.erase(it);
        m_locationToId.erase(logicalName);
        m_fileHelperOverride.erase(logicalName);
        lock.unlock();
        m_fslogic->sendFileNotUsed(logicalName);
    }
}

void StorageMapper::helperOverride(const std::string &filePath,
                                   const StorageInfo &mapping)
{
    boost::lock_guard<boost::shared_mutex> guard{m_fileMappingMutex};
    m_fileHelperOverride[filePath] = mapping;
}

void StorageMapper::resetHelperOverride(const std::string &filePath)
{
    boost::lock_guard<boost::shared_mutex> guard{m_fileMappingMutex};
    m_fileHelperOverride.erase(filePath);
}

void StorageMapper::clearMappings(const std::string &logicalName)
{
    boost::lock_guard<boost::shared_mutex> guard{m_fileMappingMutex};
    const auto it = m_locationToId.find(logicalName);
    if (it != m_locationToId.end()) {
        m_fileMapping.erase(it->second);
        m_locationToId.erase(it);
    }
}

void StorageMapper::removeExpiredLocationMappings(const std::string &location)
{
    boost::lock_guard<boost::shared_mutex> guard{m_fileMappingMutex};
    const auto it = mappingFromLogicalName(location);
    if (it != m_fileMapping.end() &&
        (*it).second.validTo <= std::chrono::steady_clock::now()) {

        LOG(INFO) << "Removing old location mapping for file: " << location;
        m_fileMapping.erase(it);
        m_locationToId.erase(location);
        m_fileHelperOverride.erase(location);

    } else if (it != m_fileMapping.end()) {

        LOG(INFO) << "Recheduling old location mapping removal for file: "
                  << location;

        const auto when = std::chrono::duration_cast<std::chrono::milliseconds>(
            it->second.validTo - std::chrono::steady_clock::now());

        m_context.lock()->scheduler()->schedule(
            when, std::bind(&StorageMapper::removeExpiredLocationMappings,
                            shared_from_this(), location));
    }
}

void StorageMapper::renewLocationMapping(const std::string &location)
{
    LOG(INFO) << "Renewing file mapping for file: " << location;

    const auto validity =
        std::chrono::seconds{m_fslogic->renewFileLocation(location)};

    if (validity <= std::chrono::seconds{0}) {
        LOG(WARNING) << "Renewing file mapping for file: " << location
                     << " failed";
        return;
    }

    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    const auto it = mappingFromLogicalName(location);
    if (it != m_fileMapping.end()) {
        it->second.validTo = std::chrono::steady_clock::now() + validity;

        LOG(INFO) << "Renewed location mapping for file: " << location
                  << ". New validity: time + " << validity.count();
    }
}

void StorageMapper::asyncGetFileLocation(const std::string &location)
{
    findLocation(location);
}

bool StorageMapper::handlePushMessage(const Answer &answer)
{
    if (!boost::algorithm::iequals(answer.message_type(),
                                   BlocksAvailable::descriptor()->name())) {
        return true;
    }

    BlocksAvailable msg;
    if (!msg.ParseFromString(answer.worker_answer())) {
        LOG(WARNING) << "Received malformed BlocksAvailable message from the "
                        "provider";
        return true;
    }

    LOG(INFO) << "Adding blocks for file: {storageId: '" << msg.storage_id()
              << "', fileId: '" << msg.file_id() << "'}";

    boost::lock_guard<boost::shared_mutex> guard{m_fileMappingMutex};
    const auto it =
        m_fileMapping.find(std::make_pair(msg.storage_id(), msg.file_id()));

    if (it == m_fileMapping.end()) {
        LOG(WARNING) << "No file mapping found identified by {storageId: '"
                     << msg.storage_id() << "'', fileId: '" << msg.file_id()
                     << "'";
        return true;
    }

    for (auto &block : msg.blocks()) {
        const auto begin = static_cast<off_t>(block.offset());
        const auto end = begin + static_cast<off_t>(block.size());
        it->second.blocks +=
            icl::discrete_interval<off_t>::right_open(begin, end);
    }

    m_newBlocksCondition.notify_all();

    return true;
}

bool StorageMapper::waitForBlock(const std::string &logicalName,
                                 const off_t offset)
{
    LOG(INFO) << "Waiting for block of '" << logicalName << "' at offset "
              << offset;

    boost::shared_lock<boost::shared_mutex> lock{m_fileMappingMutex};

    const auto pred = [&] {
        const auto it = mappingFromLogicalName(logicalName);
        if (it == m_fileMapping.end())
            throw OneException{
                VEIO, "Waiting for file block failed: no file mapping found!"};

        return it->second.blocks.find(offset) != it->second.blocks.end();
    };

    return m_newBlocksCondition.wait_for(lock, WAIT_FOR_BLOCK_TIMEOUT, pred);
}

LocationInfo StorageMapper::retrieveLocationInfo(const std::string &logicalName,
                                                 const bool useCluster,
                                                 const bool forceClusterProxy)
{
    boost::shared_lock<boost::shared_mutex> lock{m_fileMappingMutex};
    auto it = mappingFromLogicalName(logicalName);

    if (it != m_fileMapping.end()) {
        boost::shared_lock<boost::shared_mutex> sLock{m_storageMappingMutex};
        auto it1 = m_storageMapping.find(it->second.storageId);
        sLock.unlock();

        if (forceClusterProxy && useCluster &&
            (it1 == m_storageMapping.end() ||
             it1->second.storageHelperName != CLUSTER_PROXY_HELPER)) {
            lock.unlock();
            findLocation(logicalName, UNSPECIFIED_MODE, forceClusterProxy);
            lock.lock();

            it = mappingFromLogicalName(logicalName);
        }
    }

    if (it == m_fileMapping.end()) {
        if (!useCluster)
            throw OneException(VEIO, "cannot find file mapping in cache but "
                                     "using cluster is not allowed in this "
                                     "context");

        lock.unlock();
        findLocation(logicalName, UNSPECIFIED_MODE, forceClusterProxy);
        lock.lock();

        const auto lastTryIt = mappingFromLogicalName(logicalName);
        if (lastTryIt != m_fileMapping.end())
            return lastTryIt->second;

        throw OneException(VEIO, "cannot find file mapping (cluster was used)");
    }

    return it->second;
}

StorageInfo StorageMapper::retrieveStorageInfo(const LocationInfo &locationInfo)
{
    boost::shared_lock<boost::shared_mutex> sLock{m_storageMappingMutex};
    try {
        return m_storageMapping.at(locationInfo.storageId);
    }
    catch (const std::out_of_range &) {
        throw OneException(VEIO, "cannot find storage information");
    }
}

void StorageMapper::addFileMapping(const std::string &logicalName,
                                   const FileLocation &location)
{
    const std::chrono::seconds validity{location.validity()};
    std::pair<std::uint32_t, std::string> locationId{location.storage_id(),
                                                     location.file_id()};

    LocationInfo info;
    info.fileId = location.file_id();
    info.storageId = location.storage_id();
    info.validTo = std::chrono::steady_clock::now() + validity;

    for (auto &block : location.available()) {
        const auto begin = static_cast<off_t>(block.offset());
        const auto end = begin + static_cast<off_t>(block.size());
        info.blocks += icl::discrete_interval<off_t>::right_open(begin, end);
    }

    m_context.lock()->scheduler()->schedule(
        validity, std::bind(&StorageMapper::removeExpiredLocationMappings,
                            shared_from_this(), logicalName));

    m_context.lock()->scheduler()->schedule(
        validity - RENEW_LOCATION_MAPPING_TIME,
        std::bind(&StorageMapper::renewLocationMapping, shared_from_this(),
                  logicalName));

    boost::lock_guard<boost::shared_mutex> guard{m_fileMappingMutex};
    const auto previous = m_fileMapping.lower_bound(locationId);
    if (previous != m_fileMapping.end() && previous->first == locationId) {
        info.opened = previous->second.opened;
        previous->second = std::move(info);
    } else {
        m_locationToId.emplace(logicalName, locationId);
        m_fileMapping.emplace_hint(previous, std::move(locationId),
                                   std::move(info));
    }
}

void StorageMapper::addStorageMapping(const FileLocation &location)
{
    StorageInfo info;
    info.last_updated =
        std::chrono::steady_clock::now(); /// @todo: last_updated field should
                                          /// be fetched from cluster

    if (location.has_storage_helper_name())
        info.storageHelperName = location.storage_helper_name();
    info.storageHelperArgs.reserve(location.storage_helper_args_size());
    for (int i = 0; i < location.storage_helper_args_size(); ++i)
        info.storageHelperArgs.emplace(
            helpers::srvArg(i), boost::any{location.storage_helper_args(i)});

    boost::lock_guard<boost::shared_mutex> guard{m_storageMappingMutex};
    m_storageMapping.emplace(location.storage_id(), std::move(info));
}

auto StorageMapper::mappingFromLogicalName(const std::string &logicalName)
    -> decltype(m_fileMapping)::iterator
{
    const auto &idIt = m_locationToId.find(logicalName);
    if (idIt == m_locationToId.end())
        return m_fileMapping.end();

    return m_fileMapping.find(idIt->second);
}

StorageInfo::StorageInfo(std::string helperName,
                         helpers::IStorageHelper::ArgsMap helperArgs)
    : last_updated{std::chrono::steady_clock::now()}
    , storageHelperName{std::move(helperName)}
    , storageHelperArgs{std::move(helperArgs)}
{
}

} // namespace client
} // namespace one
