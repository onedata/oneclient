/**
 * @file emitter.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EMITTERS_EMITTER_H
#define ONECLIENT_EVENTS_EMITTERS_EMITTER_H

#include "events/declarations.h"

namespace one {
namespace client {
namespace events {

/**
 * @c Emitter class represents an abstract event stream layer responsible for
 * declaring emission ready status of a stream. It provides an interface for
 * concrete emitters.
 */
template <class T> class Emitter {
public:
    virtual ~Emitter() = default;

    /**
     * Updates emitter state base on provided event and returns it for further
     * processing.
     * @param event An event to be prcessed.
     * @return An event for further processing.
     */
    virtual EventPtr<T> process(EventPtr<T> event) = 0;

    /**
     * Checks whether emitter declares an event stream ready for emission of
     * aggregated events.
     * @return An emission ready status.
     */
    virtual bool ready() = 0;

    /**
     * Resets emitter state and invalidates emission ready status.
     */
    virtual void reset() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_EMITTER_H
