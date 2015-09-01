/**
 * @file fileBlock.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_BLOCK_H
#define ONECLIENT_MESSAGES_FUSE_FILE_BLOCK_H

#include <string>
#include <ostream>

namespace one {
namespace messages {
namespace fuse {

class FileBlock {
public:
    FileBlock() = default;

    FileBlock(std::string storageId_, std::string fileId_)
        : m_storageId{std::move(storageId_)}
        , m_fileId{std::move(fileId_)}
    {
    }

    bool operator==(const FileBlock &other) const
    {
        return m_storageId == other.m_storageId && m_fileId == other.m_fileId;
    }

    const std::string &storageId() const { return m_storageId; }
    const std::string &fileId() const { return m_fileId; }

    FileBlock &operator+=(const FileBlock &other)
    {
        if (m_fileId.empty() || m_storageId.empty()) {
            m_fileId = other.m_fileId;
            m_storageId = other.m_storageId;
        }
        return *this;
    }

private:
    std::string m_storageId;
    std::string m_fileId;
};

inline std::ostream &operator<<(std::ostream &o, const FileBlock &fileBlock)
{
    return o << "(" << fileBlock.storageId() << ", " << fileBlock.fileId()
             << ")";
}

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_BLOCK_H
