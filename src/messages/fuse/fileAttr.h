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
 * The FileAttr class represents server-sent file attributes.
 */
class FileAttr : public FuseResponse {
public:
    enum class FileType { regular, directory, link };

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileAttr counterpart.
     */
    FileAttr(std::unique_ptr<ProtocolServerMessage> serverMessage);

    const std::string &uuid() const;
    mode_t mode() const;
    uid_t uid() const;
    gid_t gid() const;
    std::chrono::system_clock::time_point atime() const;
    std::chrono::system_clock::time_point mtime() const;
    std::chrono::system_clock::time_point ctime() const;
    FileType type() const;
    off_t size() const;

    std::string toString() const override;

private:
    clproto::FileAttr m_fileAttr;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_FUSE_MESSAGES_FILE_ATTR_H
