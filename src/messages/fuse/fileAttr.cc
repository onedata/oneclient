/**
 * @file fileAttr.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/fuse/fileAttr.h"

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

    deserialize(serverMessage->fuse_response().file_attr());
}

FileAttr::FileAttr(const one::clproto::FileAttr &message)
{
    deserialize(message);
}

const FileAttr::Key &FileAttr::key() const { return m_uuid; }

const std::string &FileAttr::uuid() const { return m_uuid; }

mode_t FileAttr::mode() const { return m_mode; }

void FileAttr::mode(const mode_t mode_) { m_mode = mode_; }

uid_t FileAttr::uid() const { return m_uid; }

void FileAttr::uid(const uid_t uid_) { m_uid = uid_; }

gid_t FileAttr::gid() const { return m_gid; }

void FileAttr::gid(const gid_t gid_) { m_gid = gid_; }

std::chrono::system_clock::time_point FileAttr::atime() const
{
    return m_atime;
}

void FileAttr::atime(std::chrono::system_clock::time_point time)
{
    m_atime = time;
}

std::chrono::system_clock::time_point FileAttr::mtime() const
{
    return m_mtime;
}

void FileAttr::mtime(std::chrono::system_clock::time_point time)
{
    m_mtime = time;
}

std::chrono::system_clock::time_point FileAttr::ctime() const
{
    return m_ctime;
}

void FileAttr::ctime(std::chrono::system_clock::time_point time)
{
    m_ctime = time;
}

FileAttr::FileType FileAttr::type() const { return m_type; }

off_t FileAttr::size() const { return m_size; }

void FileAttr::size(const off_t size_) { m_size = size_; }

void FileAttr::aggregate(FileAttrPtr fileAttr)
{
    m_mode = fileAttr->m_mode;
    m_uid = fileAttr->m_uid;
    m_gid = fileAttr->m_gid;
    m_atime = fileAttr->m_atime;
    m_mtime = fileAttr->m_mtime;
    m_ctime = fileAttr->m_ctime;
    m_size = fileAttr->m_size;
    m_type = fileAttr->m_type;
}

std::string FileAttr::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileAttr', uuid: '" << m_uuid << "', name: '" << m_name
           << "', mode: " << m_mode << ", uid: " << m_uid << ", gid: " << m_gid
           << ", atime: " << std::chrono::system_clock::to_time_t(m_atime)
           << ", mtime: " << std::chrono::system_clock::to_time_t(m_mtime)
           << ", ctime: " << std::chrono::system_clock::to_time_t(m_ctime)
           << ", size: " << m_size << ", type: ";

    switch (m_type) {
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

void FileAttr::deserialize(const ProtocolMessage &message)
{
    m_uuid = message.uuid();
    m_name = message.name();
    m_mode = static_cast<mode_t>(message.mode());
    m_uid = static_cast<uid_t>(message.uid());
    m_gid = static_cast<gid_t>(message.gid());
    m_atime = std::chrono::system_clock::from_time_t(message.atime());
    m_mtime = std::chrono::system_clock::from_time_t(message.mtime());
    m_ctime = std::chrono::system_clock::from_time_t(message.ctime());
    m_size = message.size();

    if (message.type() == clproto::FileType::DIR)
        m_type = FileType::directory;
    else if (message.type() == clproto::FileType::REG)
        m_type = FileType::regular;
    else if (message.type() == clproto::FileType::LNK)
        m_type = FileType::link;
    else
        throw std::system_error{
            std::make_error_code(std::errc::protocol_error), "bad filetype"};
}

} // namespace fuse
} // namespace messages
} // namespace one
