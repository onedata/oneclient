/**
 * @file fileAttr.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileAttr.h"

#include "messages.pb.h"

#include <sys/types.h>

#include <sstream>
#include <system_error>
#include <tuple>

namespace one {
namespace messages {
namespace fuse {

FileAttr::FileAttr(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse(serverMessage)
{
    if (!serverMessage->fuse_response().has_file_attr())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "file_attr field missing"};

    m_fileAttr = std::move(serverMessage->fuse_response().file_attr());
}

std::chrono::system_clock::time_point FileAttr::atime() const
{
    return std::chrono::system_clock::from_time_t(m_fileAttr.atime());
}

void FileAttr::atime(std::chrono::system_clock::time_point t)
{
    m_fileAttr.set_atime(std::chrono::system_clock::to_time_t(t));
}

std::chrono::system_clock::time_point FileAttr::mtime() const
{
    return std::chrono::system_clock::from_time_t(m_fileAttr.mtime());
}

void FileAttr::mtime(std::chrono::system_clock::time_point t)
{
    m_fileAttr.set_mtime(std::chrono::system_clock::to_time_t(t));
}

std::chrono::system_clock::time_point FileAttr::ctime() const
{
    return std::chrono::system_clock::from_time_t(m_fileAttr.ctime());
}

FileAttr::FileType FileAttr::type() const
{
    switch (m_fileAttr.type()) {
        case clproto::FileType::DIR:
            return FileType::directory;
        case clproto::FileType::REG:
            return FileType::regular;
        case clproto::FileType::LNK:
            return FileType::link;
        default:
            throw std::system_error{
                std::make_error_code(std::errc::protocol_error),
                "bad filetype"};
    }
}

std::string FileAttr::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileAttr', mode: " << mode() << ", uid: " << uid()
           << ", gid: " << gid() << ", atime: " << m_fileAttr.atime()
           << ", mtime: " << m_fileAttr.mtime()
           << ", ctime: " << m_fileAttr.ctime() << ", size: " << size()
           << ", type: ";

    switch (type()) {
        case FileType::directory:
            stream << "directory";
            break;
        case FileType::regular:
            stream << "regular";
            break;
        case FileType::link:
            stream << "link";
            break;
    }

    return stream.str();
}

void FileAttr::size(const off_t s) { m_fileAttr.set_size(s); }

} // namespace fuse
} // namespace messages
} // namespace one
