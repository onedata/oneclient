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

std::string FileLocationChanged::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileLocationChanged', file location: "
           << m_fileLocation->toString();
    return stream.str();
}

void FileLocationChanged::aggregate(EventPtr<FileLocationChanged> event)
{
    m_fileLocation = std::move(event->m_fileLocation);
}

} // namespace events
} // namespace client
} // namespace one
