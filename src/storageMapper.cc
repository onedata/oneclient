/**
 * @file storageMapper.cc
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "storageMapper.h"
#include "veilfs.h"

#include "glog/logging.h"

#include <ctime>
#include <arpa/inet.h>

using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;

namespace veil {
namespace client {

StorageMapper::StorageMapper(boost::shared_ptr<FslogicProxy> fslogicProxy) : 
    m_fslogic(fslogicProxy)
{
}

StorageMapper::~StorageMapper()
{
}

pair<locationInfo, storageInfo> StorageMapper::getLocationInfo(string logicalName, bool useCluster)
{
    AutoLock lock(m_fileMappingLock, READ_LOCK);
    map<string, locationInfo>::iterator it = m_fileMapping.find(logicalName);
    if(it == m_fileMapping.end())
    {
        if(!useCluster)
            throw VeilException(VEIO, "cannot find file mapping in cache but using cluster is not allowed in this context");

        lock.release();
        string status = findLocation(logicalName);
        lock.lock();

        it = m_fileMapping.find(logicalName);
        if(it == m_fileMapping.end())
            throw VeilException(VEIO, "cannot find file mapping (cluster was used)");
    }

    locationInfo location = it->second;

    // Find matching storage info storage
    AutoLock sLock(m_storageMappingLock, READ_LOCK);
    map<int, storageInfo>::iterator it1 = m_storageMapping.find(location.storageId);
    if(it1 == m_storageMapping.end())
        throw VeilException(VEIO, "cannot find storage information");

    LOG(INFO) << "found mapping for file: " << logicalName << " fileId: " << location.fileId << " storageId: " << location.storageId;
    return make_pair(it->second, it1->second);
}

string StorageMapper::findLocation(string logicalName)
{
    LOG(INFO) << "quering cluster about storage mapping for file: " << logicalName;
    FileLocation location;
    if(m_fslogic->getFileLocation(logicalName, location))
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

void StorageMapper::addLocation(string logicalName, FileLocation location)
{
    LOG(INFO) << "adding location for file '" << logicalName << "', fileId: " << location.file_id() << " storageId: " << location.storage_id() << ", validTo: " << time(NULL) << " + " << ntohl(location.validity());

    locationInfo info;
    info.validTo = time(NULL) + location.validity();
    info.storageId = location.storage_id();
    info.fileId = location.file_id();

    storageInfo storageInfo;
    storageInfo.last_updated = time(NULL); /// @todo: last_updated field should be fetched from cluster
    if(location.has_storage_helper_name())
        storageInfo.storageHelperName = location.storage_helper_name();

    for(int i = 0; i < location.storage_helper_args_size(); ++i)
        storageInfo.storageHelperArgs.push_back(location.storage_helper_args(i));

    info.opened = 0;

    AutoLock lock(m_fileMappingLock, WRITE_LOCK);
    m_fileMapping[logicalName] = info;
    AutoLock sLock(m_storageMappingLock, WRITE_LOCK);
    m_storageMapping[info.storageId] = storageInfo;

    VeilFS::getScheduler()->addTask(Job(info.validTo, shared_from_this(), TASK_REMOVE_EXPIRED_LOCATON_MAPPING, logicalName));
    VeilFS::getScheduler()->addTask(Job(info.validTo - RENEW_LOCATION_MAPPING_TIME, shared_from_this(), TASK_RENEW_LOCATION_MAPPING, logicalName));
}

void StorageMapper::openFile(string logicalName)
{
    LOG(INFO) << "marking file '" << logicalName << "' as open";

    AutoLock lock(m_fileMappingLock, WRITE_LOCK);
    map<string, locationInfo>::iterator it = m_fileMapping.find(logicalName);
    if(it != m_fileMapping.end()){
         it->second.opened++;
    }
}

void StorageMapper::releaseFile(string logicalName)
{
    LOG(INFO) << "marking file '" << logicalName << "' closed";

    AutoLock lock(m_fileMappingLock, WRITE_LOCK);
    map<string, locationInfo>::iterator it = m_fileMapping.find(logicalName);
    if(it != m_fileMapping.end()){
        if(it->second.opened > 0){
            it->second.opened--;
            if(it->second.opened == 0){
                //locationMapping.erase(logicalName); // TODO: it should be done automatically
            }
        }
    }
}

bool StorageMapper::runTask(TaskID taskId, string arg0, string arg1, string arg3)
{
    map<string, locationInfo>::iterator it;
    int validity;
    AutoLock lock(m_fileMappingLock, WRITE_LOCK);
    switch(taskId)
    {
        case TASK_REMOVE_EXPIRED_LOCATON_MAPPING:
            it = m_fileMapping.find(arg0);
            if(it != m_fileMapping.end() && (*it).second.validTo <= time(NULL)) 
            {
                LOG(INFO) << "Removing old location mapping for file: " << arg0;
                m_fileMapping.erase(arg0);
            }
            else if(it != m_fileMapping.end()) 
            {
                LOG(INFO) << "Recheduling old location mapping removal for file: " << arg0;
                VeilFS::getScheduler()->addTask(Job((*it).second.validTo, shared_from_this(), TASK_REMOVE_EXPIRED_LOCATON_MAPPING, arg0));
            }

            return true;
        case TASK_RENEW_LOCATION_MAPPING:
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
        case TASK_ASYNC_GET_FILE_LOCATION:
            (void) findLocation(arg0);
            return true;
        default:
            return false;
    }
}

} // namespace client
} // namespace veil
