/**
 * @file localStorageManager.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef LOCAL_STORAGE_MANAGER_HH
#define LOCAL_STORAGE_MANAGER_HH

#ifdef __APPLE__
#include <sys/mount.h>
#else
#include <mntent.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <utility>
#include <boost/filesystem.hpp>
#include <boost/optional/optional.hpp>

#define MOUNTS_INFO_FILE_PATH           "/proc/mounts"
#define STORAGE_INFO_FILENAME           "vfs_storage.info"

namespace veil {
namespace client {

/**
 * Local Storage Manager.
 * Inform server about directly accessible storage.
 */
class LocalStorageManager
{
public:

    std::vector< boost::filesystem::path > getMountPoints();                            ///< Returns vector of mount points available in the system

    std::vector< std::pair <int, std::string> >
    getClientStorageInfo(const std::vector< boost::filesystem::path > &mountPoints);    ///< Returns vector of pairs of storage id and absolute path to storage that is directly accessible by a client

    bool sendClientStorageInfo
    (const std::vector< std::pair<int, std::string> > &clientStorageInfo);              ///< Informs server about storage that is directly accessible to the client

    LocalStorageManager();
    virtual ~LocalStorageManager();

protected:

    std::vector< std::pair<int, std::string> > parseStorageInfo(const boost::filesystem::path &mountPoint);                         ///< Returns vector of pairs of storage id and absolute path to storage read from vfs_storage.info file located at mount point
    boost::optional< std::pair<std::string, std::string> > createStorageTestFile(const int storageId);                              ///< Creates test file on storage in client home directory and returns path to created file and its content
    bool hasClientStorageReadPermission(const std::string &storagePath, const std::string &relativePath, const std::string &text);  ///< Checks whether client can read specified file on storage
    bool hasClientStorageWritePermission(const int storageId, const std::string &absolutePath, const std::string &relativePath);    ///< Checks whether client can write to specified file on storage
};

} // namespace client
} // namespace veil

#endif // CONFIG_HH
