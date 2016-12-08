/**
 * @file stream.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_STREAMS_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_STREAM_H

#include "events/declarations.h"

namespace one {
namespace client {
namespace events {

/**
 * @c Stream class represents an abstract events processing channel. It provides
 * an interface for concrete streams.
 */
class Stream {
public:
    virtual ~Stream() = default;

    /**
     * Processes an event in the stream.
     * @param event An event to be processed by the stream.
     */
    virtual void process(EventPtr<> event) = 0;

    /**
     * Requests handling of events aggregated in the stream.
     */
    virtual void flush() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_STREAM_H
