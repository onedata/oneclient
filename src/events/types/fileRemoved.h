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

class FileRemoved : public SingleEvent {
    using ProtocolMessage = clproto::FileRemovedEvent;

public:
    FileRemoved(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    const std::string &fileUuid() const;

    std::string toString() const override;

private:
    std::string m_fileUuid;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_REMOVED_H
