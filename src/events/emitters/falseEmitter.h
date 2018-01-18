/**
 * @file falseEmitter.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EMITTERS_FALSE_EMITTER_H
#define ONECLIENT_EVENTS_EMITTERS_FALSE_EMITTER_H

#include "emitter.h"

namespace one {
namespace client {
namespace events {

/**
 * @c FalseEmitter ignores processed events and never declares emission ready
 * status. It is intended to be the last element of the emitters chain.
 */
template <class T> class FalseEmitter : public Emitter<T> {
public:
    /**
     * Ignores processed events.
     * @see Emitter:process(EventPtr<T> event)
     */
    EventPtr<T> process(EventPtr<T> event) override;

    /**
     * Always returns false.
     * @see Emitter::ready()
     */
    bool ready() override;

    /**
     * Ignores this call.
     * @see Emitter::reset()
     */
    void reset() override;
};

template <class T> EventPtr<T> FalseEmitter<T>::process(EventPtr<T> event)
{
    LOG_FCALL();

    return std::move(event);
}

template <class T> bool FalseEmitter<T>::ready() { return false; }

template <class T> void FalseEmitter<T>::reset() {}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_FALSE_EMITTER_H
