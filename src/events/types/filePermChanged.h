/**
 * @file filePermChanged.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_PERM_CHANGED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_PERM_CHANGED_EVENT_H

#include "singleEvent.h"

namespace one {
namespace clproto {
class FilePermChangedEvent;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c FilePermChanged class represents a change of file permission in the
 * system.
 */
class FilePermChanged : public SingleEvent {
    using ProtocolMessage = clproto::FilePermChangedEvent;

public:
    /**
     * Constructor.
     * @param msg A Protocol Buffers message representing @c FilePermChanged
     * counterpart.
     */
    FilePermChanged(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    /**
     * @return An UUID of a file which permission has been changed.
     */
    const std::string &fileUuid() const;

    std::string toString() const override;

private:
    std::string m_fileUuid;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_PERM_CHANGED_EVENT_H
