/**
 * @file fileAttr.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_ATTR_H
#define ONECLIENT_MESSAGES_FUSE_FILE_ATTR_H

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
    const std::string &uuid() const { return m_fileAttr.uuid(); }

    /**
     * @return name of the file.
     */
    const std::string &name() const { return m_fileAttr.name(); }

    /**
     * Sets a new filename.
     * @param newName New name od the file.
     */
    const void name(std::string newName) { m_fileAttr.set_name(newName); }

    /**
     * @return File access mode.
     */
    mode_t mode() const { return m_fileAttr.mode(); }

    /**
     * Sets a new mode.
     * @param newMode The mode to set.
     */
    void mode(const mode_t newMode) { m_fileAttr.set_mode(newMode); }

    /**
     * @return ID of the file's owner.
     */
    uid_t uid() const { return m_fileAttr.uid(); }

    /**
     * @return Group ID of the file's owner.
     */
    gid_t gid() const { return m_fileAttr.gid(); }

    /**
     * @return Last access time to the file.
     */
    std::chrono::system_clock::time_point atime() const;

    /**
     * Set file's last access time.
     * @param t The access time to set.
     */
    void atime(std::chrono::system_clock::time_point t);

    /**
     * @return Last modification time of the file.
     */
    std::chrono::system_clock::time_point mtime() const;

    /**
     * Set file's last modification time.
     * @param t The modification time to set.
     */
    void mtime(std::chrono::system_clock::time_point t);

    /**
     * @return File's change time.
     */
    std::chrono::system_clock::time_point ctime() const;

    /**
     * @return Type of the file (regular, link, directory).
     */
    FileType type() const;

    /**
     * @return Size of the file.
     */
    off_t size() const { return m_fileAttr.size(); }

    std::string toString() const override;

private:
    clproto::FileAttr m_fileAttr;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_ATTR_H
