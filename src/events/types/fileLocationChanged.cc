/**
 * @file fileLocationChanged.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileLocationChanged.h"
#include "messages/fuse/fileLocation.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileLocationChanged::FileLocationChanged(const ProtocolMessage &msg)
    : m_fileLocation{std::make_unique<FileLocation>(msg.file_location())}
{
    if (msg.has_change_beg_offset())
        m_changeBegOffset.reset(msg.change_beg_offset());
    if (msg.has_change_end_offset())
        m_changeEndOffset.reset(msg.change_end_offset());
}

StreamKey FileLocationChanged::streamKey() const
{
    return StreamKey::FILE_LOCATION_CHANGED;
}

const AggregationKey &FileLocationChanged::aggregationKey() const
{
    return m_fileLocation->uuid();
}

const FileLocation &FileLocationChanged::fileLocation() const
{
    return *m_fileLocation;
}

boost::optional<off_t> FileLocationChanged::changeStartOffset() const
{
    return m_changeBegOffset;
}

boost::optional<off_t> FileLocationChanged::changeEndOffset() const
{
    return m_changeEndOffset;
}

std::string FileLocationChanged::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileLocationChanged', file location: "
           << m_fileLocation->toString();
    if (m_changeBegOffset)
        stream << ", change_beg_offset: " << *m_changeBegOffset;
    if (m_changeEndOffset)
        stream << ", change_end_offset: " << *m_changeEndOffset;
    return stream.str();
}

void FileLocationChanged::aggregate(EventPtr<FileLocationChanged> event)
{
    if (m_changeBegOffset && m_changeEndOffset) {
        m_fileLocation->updateInRange(
            *m_changeBegOffset, *m_changeEndOffset, *(event->m_fileLocation));
    }
    else
        m_fileLocation = std::move(event->m_fileLocation);
}

} // namespace events
} // namespace client
} // namespace one
