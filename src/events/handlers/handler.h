/**
 * @file handler.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_HANDLERS_HANDLER_H
#define ONECLIENT_EVENTS_HANDLERS_HANDLER_H

#include "events/declarations.h"

#include <functional>
#include <vector>

namespace one {
namespace client {
namespace events {

/**
 * @c Handler class represents an abstract event stream layer responsible for
 * handling aggregated events. It provides an interface for concrete event
 * handlers.
 */
template <class T> class Handler {
public:
    virtual ~Handler() = default;

    /**
     * Handles aggregated events.
     * @param events Collection of aggregated events to be prcessed.
     */
    virtual void process(Events<T> events) = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_HANDLERS_HANDLER_H
