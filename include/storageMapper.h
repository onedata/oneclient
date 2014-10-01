/**
 * @file storageMapper.h
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_STORAGE_MAPPER_H
#define VEILCLIENT_STORAGE_MAPPER_H


#include "ISchedulable.h"

#include "helpers/IStorageHelper.h"
#include "fslogicProxy.h"

#include <boost/thread/shared_mutex.hpp>

#include <map>
#include <memory>
#include <string>
#include <boost/filesystem.hpp>

namespace veil
{

namespace protocol{ namespace fuse_messages{ class FileLocation; }}

namespace client
{

static constexpr const char *CLUSTER_PROXY_HELPER = "ClusterProxy";

class Context;
class FslogicProxy;

/**
 * Structure containing file mapping base information.
 */
struct locationInfo
{
    int storageId; ///< Storage identificator. @see StorageMapper::m_storageMapping
    std::string fileId; ///< File identificator.
                   ///< This ID should be recognisable by _storage helper_. Most commonly it's just
                   ///< file path relative to the _storage_ @see StorageMapper::m_fileMapping

    time_t validTo; ///< Mapping expiration time
    int opened;     ///< How many files are currently opened using this mapping.

    bool isValid() ///< Checks if the structure contains vaild data.
    {
        return storageId > 0;
    }
};

struct storageInfo
{
    time_t last_updated;                                ///< Last update time
    std::string storageHelperName;                      ///< Name of storage helper. @see StorageHelperFactory::getStorageHelper
    helpers::IStorageHelper::ArgsMap storageHelperArgs; ///< Arguments for storage helper. @see StorageHelperFactory::getStorageHelper

    storageInfo() {}

    storageInfo(const std::string &helperName, const helpers::IStorageHelper::ArgsMap &helperArgs)
        : last_updated(time(nullptr))
        , storageHelperName(helperName)
        , storageHelperArgs(helperArgs)
    {
    }

    bool isValid()                                      ///< Checks if the structure contains vaild data.
    {
        return storageHelperName.size() > 0 && last_updated > 0;
    }
};

class StorageMapper: public std::enable_shared_from_this<StorageMapper>
{

protected:
    std::map<int, storageInfo> m_storageMapping;            ///< Contains storage info accessd by its ID. @see storageInfo
    boost::shared_mutex m_storageMappingMutex;              ///< Mutex used while operating on StorageMapper::m_storageMapping. @see StorageMapper::m_storageMapping
    std::map<std::string, locationInfo> m_fileMapping;      ///< Contains storage info accessd by its ID. @see storageInfo
    std::map<std::string, storageInfo> m_fileHelperOverride;
    boost::shared_mutex m_fileMappingMutex;                 ///< Mutex used while operationg on StorageMapper::m_fileMapping. @see StorageMapper::m_fileMapping

    std::shared_ptr<FslogicProxy> m_fslogic;              ///< Reference to FslogicProxy instance. @see VeilFS::m_fslogic

public:

    StorageMapper(std::weak_ptr<Context> context, std::shared_ptr<FslogicProxy> fslogicProxy);
    virtual ~StorageMapper() = default;

    /**
     * Gets file location information along with storage info for storage heleper's calls.
     * @param logical_name File path (relative to VeilFS mount point)
     * @param useCluster Specify if the method should use cache only (deafault) or try quering cluster.
     * @return std::pair of locationInfo and storageInfo structs for this file
     */
    virtual std::pair<locationInfo, storageInfo> getLocationInfo(const std::string &logical_name, bool useCluster = false, bool forceClusterProxy = false);
    virtual std::string findLocation(const std::string &logicalName, const std::string &openMode = UNSPECIFIED_MODE, bool forceClusterProxy = false);///< Query cluster about file location and instert it to cache. @see StorageMapper::addLocation
    virtual void addLocation(const std::string &logicalName, const protocol::fuse_messages::FileLocation &location); ///< Cache given file location.
                                                                            ///< Insert to file location cache new FileLocation received from cluster.
    virtual void openFile(const std::string &logicalName);                  ///< Increases open file count for specified file. @see locationInfo::opened
    virtual void releaseFile(const std::string &logicalName);               ///< Decreases open file count for specified file. @see locationInfo::opened

    /**
     * Overrides helper info that shall be used for the given file.
     * @param filePath to the file
     * @param mapping to override to
     */
    virtual void helperOverride(const std::string &filePath, const storageInfo &mapping);

    /**
     * Resets helper override setup with StorageMapper::helperOverride.
     * @param filePath to the file
     */
    virtual void resetHelperOverride(const std::string &filePath);

    virtual void clearMappings(const std::string &logicalName);             ///< Clears location cache for the file.

    virtual void runTask(ISchedulable::TaskID taskId, const std::string &arg0, const std::string &arg1, const std::string &arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask

private:
    const std::weak_ptr<Context> m_context;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_STORAGE_MAPPER_H
