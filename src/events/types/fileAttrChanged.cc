/**
 * @file fileAttrChanged.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileAttrChanged.h"
#include "messages/fuse/fileAttr.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileAttrChanged::FileAttrChanged(const ProtocolMessage &msg)
    : m_fileAttr{std::make_unique<FileAttr>(msg.file_attr())}
    , m_aggregationKey{m_fileAttr->uuid().toStdString()}
{
}

StreamKey FileAttrChanged::streamKey() const
{
    return StreamKey::FILE_ATTR_CHANGED;
}

const std::string &FileAttrChanged::aggregationKey() const
{
    return m_aggregationKey;
}

const FileAttr &FileAttrChanged::fileAttr() const { return *m_fileAttr; }

std::string FileAttrChanged::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileAttrChanged', file attr: " << m_fileAttr->toString();
    return stream.str();
}

void FileAttrChanged::aggregate(EventPtr<FileAttrChanged> event)
{
    m_fileAttr = std::move(event->m_fileAttr);
}

} // namespace events
} // namespace client
} // namespace one
