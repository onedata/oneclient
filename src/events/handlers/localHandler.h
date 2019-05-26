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
#include "helpers/logging.h"

namespace one {
namespace client {
namespace events {

/**
 * @c LocalHandler is a wrapper for a custom events handling callback.
 */
template <class T> class LocalHandler : public Handler<T> {
public:
    /**
     * Constructor.
     * @param handler A custom events handling callback.
     */
    LocalHandler(EventHandler<T> handler);

    /**
     * Executes events handling callback. Skips empty events collection.
     * @see Handler::process(Events<T> events)
     */
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
    LOG_FCALL();

    if (!events.empty()) {
        m_handler(std::move(events));
    }
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_HANDLERS_LOCAL_HANDLER_H
