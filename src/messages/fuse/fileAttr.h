/**
 * @file fileAttr.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_FUSE_MESSAGES_FILE_ATTR_H
#define HELPERS_MESSAGES_FUSE_MESSAGES_FILE_ATTR_H

#include "fuseResponse.h"

#include "messages.pb.h"

#include <sys/types.h>

#include <chrono>
#include <cstdint>
#include <memory>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The FileAttr class represents server-sent attributes of a file.
 */
class FileAttr : public FuseResponse {
public:
    enum class FileType { regular, directory, link };

    FileAttr() = default;

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileAttr counterpart.
     */
    FileAttr(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return UUID of the file.
     */
    const std::string &uuid() const;

    /**
     * @return File access mode.
     */
    mode_t mode() const;

    /**
     * Sets a new mode.
     * @param newMode The mode to set.
     */
    void mode(const mode_t newMode);

    /**
     * @return ID of the file's owner.
     */
    uid_t uid() const;

    /**
     * @return Group ID of the file's owner.
     */
    gid_t gid() const;

    /**
     * @return Last access time to the file.
     */
    std::chrono::system_clock::time_point atime() const;

    /**
     * @return Last modification time of the file.
     */
    std::chrono::system_clock::time_point mtime() const;

    /**
     * @return File's creation time.
     */
    std::chrono::system_clock::time_point ctime() const;

    /**
     * @return Type of the file (regular, link, directory).
     */
    FileType type() const;

    /**
     * @return Size of the file.
     */
    off_t size() const;

    std::string toString() const override;

private:
    clproto::FileAttr m_fileAttr;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_FUSE_MESSAGES_FILE_ATTR_H
