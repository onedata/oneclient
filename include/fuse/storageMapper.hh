/**
 * @file storageMapper.hh
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#ifndef STORAGE_MAPPER_H
#define STORAGE_MAPPER_H

#include <map>
#include <string>
#include <pthread.h>
using namespace std;

#include "fuse_messages.pb.h"
#include "fslogicProxy.hh"
#include "ISchedulable.hh"
#include "lock.hh"
#include "veilException.hh"

/**
 * Structure containing file mapping base information.
 */
typedef struct locationInfo {
    int storageId; ///< Storage identificator. @see StorageMapper::m_storageMapping
    string fileId; ///< File identificator.
                   ///< This ID should be recognisable by _storage helper_. Most commonly it's just
                   ///< file path relative to the _storage_ @see StorageMapper::m_fileMapping

    time_t validTo; ///< Mapping expiration time
    int opened; ///< How many files are currently opened using this mapping.

    bool isValid() ///< Checks if the structure contains vaild data.
    {
        return storageId > 0;
    }
} locationInfo;

typedef struct storageInfo
{
    time_t last_updated; ///< Last update time
    string storageHelperName; ///< Name of storage helper. @see StorageHelperFactory::getStorageHelper
    vector<string> storageHelperArgs; ///< Arguments for storage helper. @see StorageHelperFactory::getStorageHelper

    bool isValid() ///< Checks if the structure contains vaild data.
    {
        return storageHelperName.size() > 0 && last_updated > 0;
    }
} storageInfo;

class StorageMapper : public ISchedulable
{

private:
    map<int, storageInfo> m_storageMapping; ///< Contains storage info accessd by its ID. @see storageInfo
    ReadWriteLock m_storageMappingLock; ///< Lock used while operating on StorageMapper::m_storageMapping. @see StorageMapper::m_storageMapping
    map<string, locationInfo> m_fileMapping; ///< Contains storage info accessd by its ID. @see storageInfo
    ReadWriteLock m_fileMappingLock; ///< Lock used while operationg on StorageMapper::m_fileMapping. @see StorageMapper::m_fileMapping

    FslogicProxy& m_fslogic; ///< Reference to FslogicProxy instance. @see VeilFS::m_fslogic

public:

    StorageMapper(FslogicProxy& fslogicProxy);
    ~StorageMapper();

    /**
     * Gets file location information along with storage info for storage heleper's calls.
     * @param logical_name File path (relative to VeilFS mount point)
     * @param useCluster Specify if the method should use cache only (deafault) or try quering cluster.
     * @return std::pair of locationInfo and storageInfo structs for this file
     */
    pair<locationInfo, storageInfo> getLocationInfo(string logical_name, bool useCluster = false);
    string findLocation(string logicalName); ///< Query cluster about file location and instert it to cache. @see StorageMapper::addLocation
    void addLocation(string logicalName, FileLocation location); ///< Cache given file location.
                                                                 ///< Insert to file location cache new FileLocation received from cluster.
    void openFile(string logicalName); ///< Increases open file count for specified file. @see locationInfo::opened
    void releaseFile(string logicalName); ///< Decreases open file count for specified file. @see locationInfo::opened

    bool runTask(TaskID taskId, string arg0, string arg1, string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask

};

#endif // STORAGE_MAPPER_H
