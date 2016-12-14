/**
 * @file fileRemoved.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_REMOVED_H
#define ONECLIENT_EVENTS_TYPES_FILE_REMOVED_H

#include "singleEvent.h"

namespace one {
namespace clproto {
class FileRemovedEvent;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c FileRemoved class represents a remove file operation in the system.
 */
class FileRemoved : public SingleEvent {
    using ProtocolMessage = clproto::FileRemovedEvent;

public:
    /**
     * Constructor.
     * @param msg A Protocol Buffers message representing @c FileRemoved
     * counterpart.
     */
    FileRemoved(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    /**
     * @return An UUID of a removed file.
     */
    const std::string &fileUuid() const;

    std::string toString() const override;

private:
    std::string m_fileUuid;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_REMOVED_H
