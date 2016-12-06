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

template <class T> class Emitter {
public:
    virtual ~Emitter() = default;

    virtual EventPtr<T> process(EventPtr<T> event) = 0;

    virtual bool ready() = 0;

    virtual void reset() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_EMITTER_H
