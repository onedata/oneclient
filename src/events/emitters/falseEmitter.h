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

template <class T> class FalseEmitter : public Emitter<T> {
public:
    EventPtr<T> process(EventPtr<T> event) override;

    bool ready() override;

    void reset() override;
};

template <class T> EventPtr<T> FalseEmitter<T>::process(EventPtr<T> event)
{
    return std::move(event);
}

template <class T> bool FalseEmitter<T>::ready() { return false; }

template <class T> void FalseEmitter<T>::reset() {}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_FALSE_EMITTER_H
