/**
 * @file streams.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_STREAMS_H
#define ONECLIENT_EVENTS_STREAMS_H

#include <ostream>

namespace one {
namespace client {
namespace events {

/**
 * @c StreamKey provides a list of available event streams.
 */
enum class StreamKey {
    FILE_READ,
    FILE_WRITTEN,
    FILE_ATTR_CHANGED,
    FILE_LOCATION_CHANGED,
    FILE_PERM_CHANGED,
    FILE_REMOVED,
    FILE_RENAMED,
    QUOTA_EXCEEDED,
    TEST
};

/**
 * Overloaded operator for printing @c StreamKey name.
 */
inline std::ostream &operator<<(std::ostream &os, const StreamKey &key)
{
    switch (key) {
        case StreamKey::FILE_READ:
            return os << "FileRead";
        case StreamKey::FILE_WRITTEN:
            return os << "FileWritten";
        case StreamKey::FILE_ATTR_CHANGED:
            return os << "FileAttrChanged";
        case StreamKey::FILE_LOCATION_CHANGED:
            return os << "FileLocationChanged";
        case StreamKey::FILE_PERM_CHANGED:
            return os << "FilePermChanged";
        case StreamKey::FILE_REMOVED:
            return os << "FileRemoved";
        case StreamKey::FILE_RENAMED:
            return os << "FileRenamed";
        case StreamKey::QUOTA_EXCEEDED:
            return os << "QuotaExceeded";
        default:
            return os << "--";
    }
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_H
