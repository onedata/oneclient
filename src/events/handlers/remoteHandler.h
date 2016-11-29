/**
 * @file remoteHandler.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_HANDLERS_REMOTE_HANDLER_H
#define ONECLIENT_EVENTS_HANDLERS_REMOTE_HANDLER_H

#include "handler.h"

namespace one {
namespace client {
namespace events {

class RemoteHandler : public Handler {
public:
    RemoteHandler(SequencerStreamPtr sequencerStream);

    void process(std::vector<EventPtr> events) override;

private:
    SequencerStreamPtr m_sequencerStream;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_HANDLERS_REMOTE_HANDLER_H
