/**
 * @file localHandler.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_HANDLERS_LOCAL_HANDLER_H
#define ONECLIENT_EVENTS_HANDLERS_LOCAL_HANDLER_H

#include "handler.h"

namespace one {
namespace client {
namespace events {

class LocalHandler : public Handler {
public:
    LocalHandler(EventHandler handler);

    void process(std::vector<EventPtr> events) override;

private:
    EventHandler m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_HANDLERS_LOCAL_HANDLER_H
