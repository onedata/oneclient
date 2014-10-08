/**
 * @file storageMapper.cc
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "storageMapper.h"

#include "context.h"
#include "fslogicProxy.h"
#include "fuse_messages.pb.h"
#include "helpers/storageHelperFactory.h"
#include "logging.h"
#include "oneException.h"
#include "fsImpl.h"
#include "scheduler.h"

#include <boost/any.hpp>

#include <arpa/inet.h>

#include <ctime>

using namespace std;
using namespace one::clproto::fuse_messages;

namespace one {
namespace client {

StorageMapper::StorageMapper(std::weak_ptr<Context> context, std::shared_ptr<FslogicProxy> fslogicProxy)
    : m_fslogic(fslogicProxy)
    , m_context{std::move(context)}
{
}

pair<LocationInfo, StorageInfo> StorageMapper::getLocationInfo(const string &logicalName, bool useCluster, bool forceClusterProxy)
{
    boost::shared_lock<boost::shared_mutex> lock{m_fileMappingMutex};
    auto it = m_fileMapping.find(logicalName);

    if(it != m_fileMapping.end())
    {
        boost::shared_lock<boost::shared_mutex> sLock{m_storageMappingMutex};
        auto it1 = m_storageMapping.find(it->second.storageId);
        sLock.unlock();

        if(forceClusterProxy && useCluster && (it1 == m_storageMapping.end() || it1->second.storageHelperName != CLUSTER_PROXY_HELPER))
        {
            lock.unlock();
            findLocation(logicalName, UNSPECIFIED_MODE, forceClusterProxy);
            lock.lock();

            it = m_fileMapping.find(logicalName);
        }
    }

    if(it == m_fileMapping.end())
    {
        if(!useCluster)
            throw OneException(VEIO, "cannot find file mapping in cache but using cluster is not allowed in this context");

        lock.unlock();
        findLocation(logicalName, UNSPECIFIED_MODE, forceClusterProxy);
        lock.lock();

        it = m_fileMapping.find(logicalName);
        if(it == m_fileMapping.end())
            throw OneException(VEIO, "cannot find file mapping (cluster was used)");
    }

    LocationInfo location = it->second;

    // Find matching storage info storage

    boost::shared_lock<boost::shared_mutex> sLock{m_storageMappingMutex};
    auto it1 = m_storageMapping.find(location.storageId);
    if(it1 == m_storageMapping.end())
        throw OneException(VEIO, "cannot find storage information");

    return make_pair(std::move(location), it1->second);
}

string StorageMapper::findLocation(const string &logicalName, const string &openMode, bool forceClusterProxy)
{
    LOG(INFO) << "quering cluster about storage mapping for file: " << logicalName;
    FileLocation location;
    if(m_fslogic->getFileLocation(logicalName, location, openMode, forceClusterProxy))
    {
        if(location.answer() == VOK)
            addLocation(logicalName, location);
        else
            LOG(WARNING) << "Cannot get storage mapping for file: " << logicalName << " due to error: " << location.answer();
        return location.answer();
    }
    else
        return VEIO;
}

void StorageMapper::addLocation(const string &logicalName, const FileLocation &location)
{
    LOG(INFO) << "0: adding location for file '" << logicalName << "', fileId: " << location.file_id() << " storageId: " << location.storage_id() << ", validTo: " << time(NULL) << " + " << location.validity();

    const std::chrono::seconds validity{location.validity()};

    LocationInfo info;
    info.validTo = std::chrono::steady_clock::now() + validity;
    info.storageId = location.storage_id();
    info.fileId = location.file_id();

    StorageInfo storageInfo;
    storageInfo.last_updated = std::chrono::steady_clock::now(); /// @todo: last_updated field should be fetched from cluster
    if(location.has_storage_helper_name())
        storageInfo.storageHelperName = location.storage_helper_name();

    for(int i = 0; i < location.storage_helper_args_size(); ++i)
        storageInfo.storageHelperArgs.emplace(helpers::srvArg(i), boost::any{location.storage_helper_args(i)});

    LOG(INFO) << "1: adding location for file '" << logicalName << "', fileId: " << location.file_id() << " storageId: " << location.storage_id() << ", validTo: " << time(NULL) << " + " << location.validity();


    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    auto previous = m_fileMapping.find(logicalName);
    info.opened = (previous == m_fileMapping.end() ? 0 : previous->second.opened) ;
    m_fileMapping[logicalName] = info;
    LOG(INFO) << "2: adding location for file '" << logicalName << "', fileId: " << location.file_id() << " storageId: " << location.storage_id() << ", validTo: " << time(NULL) << " + " << location.validity();

    boost::lock_guard<boost::shared_mutex> sLock{m_storageMappingMutex};
    m_storageMapping[info.storageId] = storageInfo;
    LOG(INFO) << "3: adding location for file '" << logicalName << "', fileId: " << location.file_id() << " storageId: " << location.storage_id() << ", validTo: " << time(NULL) << " + " << location.validity();

    m_context.lock()->scheduler()->schedule(
                validity,
                std::bind(&StorageMapper::removeExpiredLocationMapping, shared_from_this(), logicalName));

    m_context.lock()->scheduler()->schedule(
                validity - RENEW_LOCATION_MAPPING_TIME,
                std::bind(&StorageMapper::renewLocationMapping, shared_from_this(), logicalName));
}

void StorageMapper::openFile(const string &logicalName)
{
    LOG(INFO) << "marking file '" << logicalName << "' as open";

    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    map<string, LocationInfo>::iterator it = m_fileMapping.find(logicalName);
    if(it != m_fileMapping.end()){
         it->second.opened++;
    }
}

void StorageMapper::releaseFile(const string &logicalName)
{
    LOG(INFO) << "marking file '" << logicalName << "' closed";

    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    map<string, LocationInfo>::iterator it = m_fileMapping.find(logicalName);
    if(it != m_fileMapping.end()){
        if(it->second.opened > 0){
            it->second.opened--;
            if(it->second.opened == 0) {
                m_fileMapping.erase(logicalName);
                m_fileHelperOverride.erase(logicalName);
                m_fslogic->sendFileNotUsed(logicalName);
            }
        }
    }
}


void StorageMapper::helperOverride(const std::string &filePath, const StorageInfo &mapping)
{
    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    m_fileHelperOverride[filePath] = mapping;
}


void StorageMapper::resetHelperOverride(const std::string &filePath)
{
    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    m_fileHelperOverride.erase(filePath);
}

void StorageMapper::clearMappings(const string &logicalName)
{
    m_fileMapping.erase(logicalName);
}

void StorageMapper::removeExpiredLocationMapping(const std::string &location)
{
    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    auto it = m_fileMapping.find(location);
    if(it != m_fileMapping.end() && (*it).second.validTo <= std::chrono::steady_clock::now())
    {
        LOG(INFO) << "Removing old location mapping for file: " << location;
        m_fileMapping.erase(location);
        m_fileHelperOverride.erase(location);
    }
    else if(it != m_fileMapping.end())
    {
        LOG(INFO) << "Recheduling old location mapping removal for file: " << location;
        m_context.lock()->scheduler()->schedule(
                    std::chrono::duration_cast<std::chrono::milliseconds>(it->second.validTo - std::chrono::steady_clock::now()),
                    std::bind(&StorageMapper::removeExpiredLocationMapping, shared_from_this(), location));
    }
}

void StorageMapper::renewLocationMapping(const std::string &location)
{
    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    LOG(INFO) << "Renewing file mapping for file: " << location;
    int validity = m_fslogic->renewFileLocation(location);
    if(validity <= 0)
    {
        LOG(WARNING) << "Renewing file mapping for file: " << location << " failed";
        return;
    }

    auto it = m_fileMapping.find(location);
    if(it != m_fileMapping.end())
    {
        it->second.validTo = std::chrono::steady_clock::now() + std::chrono::seconds{validity};
        LOG(INFO) << "Renewed location mapping for file: " << location << ". New validity: time + " << validity;
    }
}

void StorageMapper::asyncGetFileLocation(const std::string &location)
{
    findLocation(location);
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
