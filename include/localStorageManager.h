/**
 * @file localStorageManager.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef LOCAL_STORAGE_MANAGER_HH
#define LOCAL_STORAGE_MANAGER_HH

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <string>
#include <vector>
#include <utility>
#include <boost/shared_ptr.hpp>

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

    bool static validatePath(std::string& path);                        ///< Checks whether does not contain '..' and returns modified path without '.'
    std::vector< std::string > static getMountPoints();                 ///< Returns vector of mount points available in the system
    std::vector< std::pair <int, std::string> >
    getClientStorageInfo(std::vector< std::string > mountPoints);       ///< Returns vector of pairs of storage id and absolute path to storage that is directly accessible by a client
    bool sendClientStorageInfo
    (std::vector< std::pair<int, std::string> > clientStorageInfo);     ///< Informs server about storage that is directly accessible to the client

    LocalStorageManager();
    virtual ~LocalStorageManager();

protected:

    bool createStorageTestFile(int storageId, std::string& relativePath, std::string& text);                    ///< Creates test file on storage in client home directory and returns path to created file and its content
    bool hasClientStorageReadPermission(std::string storagePath, std::string relativePath, std::string text);   ///< Checks whether client can read specified file on storage
    bool hasClientStorageWritePermission(int storageId, std::string storagePath, std::string relativePath);     ///< Checks whether client can write to specified file on storage
};

} // namespace client
} // namespace veil

#endif // CONFIG_HH
