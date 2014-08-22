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
#include "jobScheduler.h"
#include "logging.h"
#include "veilException.h"
#include "veilfs.h"

#include <boost/any.hpp>

#include <ctime>
#include <arpa/inet.h>

using namespace std;
using namespace veil::protocol::fuse_messages;

namespace veil {
namespace client {

StorageMapper::StorageMapper(std::weak_ptr<Context> context, std::shared_ptr<FslogicProxy> fslogicProxy)
    : m_fslogic(fslogicProxy)
    , m_context{std::move(context)}
{
}

pair<locationInfo, storageInfo> StorageMapper::getLocationInfo(const string &logicalName, bool useCluster, bool forceClusterProxy)
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
            throw VeilException(VEIO, "cannot find file mapping in cache but using cluster is not allowed in this context");

        lock.unlock();
        findLocation(logicalName, UNSPECIFIED_MODE, forceClusterProxy);
        lock.lock();

        it = m_fileMapping.find(logicalName);
        if(it == m_fileMapping.end())
            throw VeilException(VEIO, "cannot find file mapping (cluster was used)");
    }

    locationInfo location = it->second;

    // Find matching storage info storage

    boost::shared_lock<boost::shared_mutex> sLock{m_storageMappingMutex};
    auto it1 = m_storageMapping.find(location.storageId);
    if(it1 == m_storageMapping.end())
        throw VeilException(VEIO, "cannot find storage information");

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

    locationInfo info;
    info.validTo = time(NULL) + location.validity();
    info.storageId = location.storage_id();
    info.fileId = location.file_id();

    storageInfo storageInfo;
    storageInfo.last_updated = time(NULL); /// @todo: last_updated field should be fetched from cluster
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

    m_context.lock()->getScheduler()->addTask(Job(info.validTo, shared_from_this(), TASK_REMOVE_EXPIRED_LOCATON_MAPPING, logicalName));
    m_context.lock()->getScheduler()->addTask(Job(info.validTo - RENEW_LOCATION_MAPPING_TIME, shared_from_this(), TASK_RENEW_LOCATION_MAPPING, logicalName));
}

void StorageMapper::openFile(const string &logicalName)
{
    LOG(INFO) << "marking file '" << logicalName << "' as open";

    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    map<string, locationInfo>::iterator it = m_fileMapping.find(logicalName);
    if(it != m_fileMapping.end()){
         it->second.opened++;
    }
}

void StorageMapper::releaseFile(const string &logicalName)
{
    LOG(INFO) << "marking file '" << logicalName << "' closed";

    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    map<string, locationInfo>::iterator it = m_fileMapping.find(logicalName);
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


void StorageMapper::helperOverride(const std::string &filePath, const storageInfo &mapping)
{
    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    m_fileHelperOverride[filePath] = mapping;
}


void StorageMapper::resetHelperOverride(const std::string &filePath)
{
    boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
    m_fileHelperOverride.erase(filePath);
}


bool StorageMapper::runTask(TaskID taskId, const string &arg0, const string &arg1, const string &arg3)
{
    map<string, locationInfo>::iterator it;
    int validity;

    switch(taskId)
    {
        case TASK_REMOVE_EXPIRED_LOCATON_MAPPING:
        {
            boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
            it = m_fileMapping.find(arg0);
            if(it != m_fileMapping.end() && (*it).second.validTo <= time(NULL))
            {
                LOG(INFO) << "Removing old location mapping for file: " << arg0;
                m_fileMapping.erase(arg0);
                m_fileHelperOverride.erase(arg0);
            }
            else if(it != m_fileMapping.end())
            {
                LOG(INFO) << "Recheduling old location mapping removal for file: " << arg0;
                m_context.lock()->getScheduler()->addTask(Job((*it).second.validTo, shared_from_this(), TASK_REMOVE_EXPIRED_LOCATON_MAPPING, arg0));
            }

            return true;
        }
        case TASK_RENEW_LOCATION_MAPPING:
        {
            boost::lock_guard<boost::shared_mutex> lock{m_fileMappingMutex};
            LOG(INFO) << "Renewing file mapping for file: " << arg0;
            if((validity = m_fslogic->renewFileLocation(arg0)) <= 0)
            {
                LOG(WARNING) << "Renewing file mapping for file: " << arg0 << " failed";
                return true;
            }
            it = m_fileMapping.find(arg0);
            if(it != m_fileMapping.end())
            {
                (*it).second.validTo = time(NULL) + validity;
                LOG(INFO) << "Renewed location mapping for file: " << arg0 << ". New validity: time + " << ntohl(validity);
            }
            return true;
        }
        case TASK_ASYNC_GET_FILE_LOCATION:
            (void) findLocation(arg0);
            return true;
        default:
            return false;
    }
}

} // namespace client
} // namespace veil
