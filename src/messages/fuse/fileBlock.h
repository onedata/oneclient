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

/**
 * @c FileBlock represents location metadata associated with a continuous range
 * of file data.
 */
class FileBlock {
public:
    FileBlock() = default;

    /**
     * Constructor.
     * @param storageId_ Storage id associated with the block.
     * @param fileId_ File id associated with the block.
     */
    FileBlock(std::string storageId_, std::string fileId_)
        : m_storageId{std::move(storageId_)}
        , m_fileId{std::move(fileId_)}
    {
    }

    /**
     * Checks two @c FileBlock instances for equality.
     * @param other The other block.
     * @return True if the instances are equal, false otherwise.
     */
    bool operator==(const FileBlock &other) const
    {
        return m_storageId == other.m_storageId && m_fileId == other.m_fileId;
    }

    /**
     * @return Storage id associated with the block.
     */
    std::string &mutableStorageId() { return m_storageId; }

    /**
     * @return Storage id associated with the block.
     */
    const std::string &storageId() const { return m_storageId; }

    /**
     * @return File id associated with the block.
     */
    std::string &mutableFileId() { return m_fileId; }

    /**
     * @return File id associated with the block.
     */
    const std::string &fileId() const { return m_fileId; }

    /**
     * Aggregates two blocks together.
     * If the block has been default-constructed, it assumes parameters of the
     * @c other block. Otherwise it keeps its own identity.
     * @param other The other block.
     * @return @c *this
     */
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

/**
 * Serializes @c FileBlock instance into an @c ostream .
 * @param o The ostream.
 * @param fileBlock The block.
 * @return @c o
 */
inline std::ostream &operator<<(std::ostream &o, const FileBlock &fileBlock)
{
    return o << "(" << fileBlock.storageId() << ", " << fileBlock.fileId()
             << ")";
}

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_BLOCK_H
