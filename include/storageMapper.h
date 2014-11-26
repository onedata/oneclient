/**
 * @file storageMapper.h
 * @author Beata Skiba
 * @author Rafal Slota
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_STORAGE_MAPPER_H
#define ONECLIENT_STORAGE_MAPPER_H

#include "helpers/IStorageHelper.h"
#include "fslogicProxy.h"

#include <boost/icl/interval_set.hpp>
#include <boost/thread/condition_variable.hpp>

#include <condition_variable>
#include <map>
#include <memory>
#include <shared_mutex>
#include <string>

namespace one {

namespace clproto {
namespace fuse_messages {
class FileLocation;
}
namespace communication_protocol {
class Answer;
}
}

namespace client {

static constexpr const char *CLUSTER_PROXY_HELPER = "ClusterProxy";
static constexpr std::chrono::seconds RENEW_LOCATION_MAPPING_TIME{30};
static constexpr std::chrono::seconds WAIT_FOR_BLOCK_TIMEOUT{10};

class Context;
class FslogicProxy;

/**
 * Structure containing file mapping base information.
 */
struct LocationInfo {
    /**
     * Storage identificator.
     * @see StorageMapper::m_storageMapping
     */
    std::uint32_t storageId;

    /**
     * File identificator.
     * This ID should be recognisable by _storage helper_. Most commonly it's
     * just file path relative to the _storage_
     * @see StorageMapper::m_fileMapping
     */
    std::string fileId;

    /**
     * Mapping expiration time.
     */
    std::chrono::steady_clock::time_point validTo;

    /**
     * Blocks available on the storage for the particular location.
     */
    boost::icl::interval_set<off_t> blocks;

    /**
     * How many files are currently opened using this mapping.
     */
    int opened = 0;
};

struct StorageInfo {
    /**
     * Last update time.
     */
    std::chrono::steady_clock::time_point last_updated;

    /**
     * Name of storage helper.
     * @see StorageHelperFactory::getStorageHelper
     */
    std::string storageHelperName;

    /**
     * Arguments for storage helper.
     * @see StorageHelperFactory::getStorageHelper
     */
    helpers::IStorageHelper::ArgsMap storageHelperArgs;

    /**
     * Constructor.
     * Creates a zero-initialized instance.
     */
    StorageInfo() = default;

    /**
     * Constructor.
     * @param helperName Name of the storage helper.
     * @param helperArgs Arguments for the storage helper.
     */
    StorageInfo(std::string helperName,
                helpers::IStorageHelper::ArgsMap helperArgs);
};

class StorageMapper : public std::enable_shared_from_this<StorageMapper> {
public:
    StorageMapper(std::weak_ptr<Context> context,
                  std::shared_ptr<FslogicProxy> fslogicProxy);
    virtual ~StorageMapper() = default;

    /**
     * Gets file location information along with storage info for storage
     * helper's calls.
     * @param logical_name File path (relative to FsImpl mount point)
     * @param useCluster Specify if the method should use cache only (default)
     * or try quering cluster.
     * @return std::pair of locationInfo and storageInfo structs for this file
     */
    virtual std::pair<LocationInfo, StorageInfo>
    getLocationInfo(const std::string &logical_name, bool useCluster = false,
                    bool forceClusterProxy = false);

    /**
     * Query cluster about file location and instert it to cache.
     * @see StorageMapper::addLocation
     */
    virtual std::string
    findLocation(const std::string &logicalName,
                 const std::string &openMode = UNSPECIFIED_MODE,
                 bool forceClusterProxy = false);

    /**
     * Cache given file location.
     * Insert to file location cache new FileLocation received from cluster.
     */
    virtual void
    addLocation(const std::string &logicalName,
                const clproto::fuse_messages::FileLocation &location);

    /**
     * Increases open file count for specified file.
     * @see locationInfo::opened
     */
    virtual void openFile(const std::string &logicalName);

    /**
     * Decreases open file count for specified file.
     * @see locationInfo::opened
     */
    virtual void releaseFile(const std::string &logicalName);

    /**
     * Overrides helper info that shall be used for the given file.
     * @param filePath to the file
     * @param mapping to override to
     */
    virtual void helperOverride(const std::string &filePath,
                                const StorageInfo &mapping);

    /**
     * Resets helper override setup with StorageMapper::helperOverride.
     * @param filePath to the file
     */
    virtual void resetHelperOverride(const std::string &filePath);

    /**
     * Clears location cache for the file.
     */
    virtual void clearMappings(const std::string &logicalName);

    /**
     * Asynchronously fetches file mappings for a given file location.
     * @param location The location of the file.
     */
    void asyncGetFileLocation(const std::string &location);

    /**
     * Handles push messages containing block updates.
     * @returns true
     * @see PushListener::subscribe()
     */
    bool
    handlePushMessage(const clproto::communication_protocol::Answer &answer);

    /**
     * Blocks current thread until a block of data of the specified file that
     * starts from a given offset becomes available.
     * @see @c one::client::WAIT_FOR_BLOCK_TIMEOUT
     * @param logicalName The logical name of the file the block is a part of.
     * @param offset The file offset.
     * @param timeout The timeout for the wait operation.
     * @returns true if the condition was met, false otherwise (after a timeout)
     */
    virtual bool waitForBlock(
        const std::string &logicalName, const off_t offset,
        const std::chrono::milliseconds timeout = WAIT_FOR_BLOCK_TIMEOUT);

    virtual bool isOpen(const std::string &logicalName);

protected:
    /**
     * Contains storage info accessd by its ID.
     * @see SstorageInfo
     */
    std::map<int, StorageInfo> m_storageMapping;

    /**
     * Mutex used while operating on StorageMapper::m_storageMapping.
     * @see StorageMapper::m_storageMapping
     */
    std::shared_timed_mutex m_storageMappingMutex;

    /**
     * Contains storage info accessd by its ID.
     * @see storageInfo
     */
    std::map<std::pair<std::uint32_t, std::string>, LocationInfo> m_fileMapping;

    /**
     * Maps a logical file name to a {storage_id, file_id} pair that uniquely
     * identify a file location.
     */
    std::map<std::string, std::pair<std::uint32_t, std::string>> m_locationToId;

    std::map<std::string, StorageInfo> m_fileHelperOverride;

    /**
     * Mutex used while operationg on StorageMapper::m_fileMapping.
     * @see StorageMapper::m_fileMapping
     */
    std::shared_timed_mutex m_fileMappingMutex;

    /**
     * Reference to FslogicProxy instance.
     * @see FsImpl::m_fslogic
     */
    std::shared_ptr<FslogicProxy> m_fslogic;

private:
    LocationInfo retrieveLocationInfo(const std::string &logicalName,
                                      const bool useCluster,
                                      const bool forceClusterProxy);

    boost::icl::discrete_interval<off_t>
    offsetSizeToInterval(const off_t offset, const size_t size) const;

    void addFileMapping(const std::string &logicalName,
                        const clproto::fuse_messages::FileLocation &location);

    void
    addStorageMapping(const clproto::fuse_messages::FileLocation &location);

    auto mappingFromLogicalName(const std::string &logicalName)
        -> decltype(m_fileMapping)::iterator;

    StorageInfo retrieveStorageInfo(const LocationInfo &locationInfo);
    void removeExpiredLocationMappings(const std::string &location);
    void renewLocationMapping(const std::string &location);

    const std::weak_ptr<Context> m_context;
    std::condition_variable_any m_newBlocksCondition;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_STORAGE_MAPPER_H
