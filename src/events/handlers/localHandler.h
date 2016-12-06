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

template <class T> class LocalHandler : public Handler<T> {
public:
    LocalHandler(EventHandler<T> handler);

    void process(Events<T> events) override;

private:
    EventHandler<T> m_handler;
};

template <class T>
LocalHandler<T>::LocalHandler(EventHandler<T> handler)
    : m_handler{std::move(handler)}
{
}

template <class T> void LocalHandler<T>::process(Events<T> events)
{
    if (!events.empty()) {
        m_handler(std::move(events));
    }
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_HANDLERS_LOCAL_HANDLER_H
