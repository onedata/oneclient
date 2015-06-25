/**
 * @file fileAttr.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileAttr.h"

#include "messages.pb.h"
#include "messages/status.h"

#include <sstream>
#include <system_error>
#include <tuple>

namespace one {
namespace messages {
namespace fuse {

FileAttr::FileAttr(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : m_serverMessage{std::move(serverMessage)}
    , m_fileAttr{m_serverMessage->fuse_response().file_attr()}
{
    auto &statusMsg = m_serverMessage->fuse_response().status();

    std::error_code code;
    std::string description;
    std::tie(code, description) = Status::translate(statusMsg);

    if (code)
        throw std::system_error{code, description};
}

std::uint32_t FileAttr::mode() const { return m_fileAttr.mode(); }

std::uint32_t FileAttr::uid() const { return m_fileAttr.uid(); }

std::uint32_t FileAttr::gid() const { return m_fileAttr.gid(); }

std::chrono::system_clock::time_point FileAttr::atime() const
{
    return std::chrono::system_clock::from_time_t(m_fileAttr.atime());
}

std::chrono::system_clock::time_point FileAttr::mtime() const
{
    return std::chrono::system_clock::from_time_t(m_fileAttr.mtime());
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

off_t FileAttr::size() const { return m_fileAttr.size(); }

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

} // namespace fuse
} // namespace messages
} // namespace one
