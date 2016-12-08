/**
 * @file sharedStream.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_STREAMS_SHARED_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_SHARED_STREAM_H

#include "stream.h"

namespace one {
namespace client {
namespace events {

/**
 * @c SharedStream is an event stream wrapper that holds reference count of
 * subscriptions associated with the stream. It allows for stream removal when
 * all subscriptions are removed.
 */
class SharedStream : public Stream {
public:
    /**
     * Constructor.
     * @param stream A wrapped @c Stream instance.
     */
    SharedStream(StreamPtr stream);

    /**
     * Forwards call to a wrapped stream.
     * @see Stream::process(EventPtr<> event)
     */
    void process(EventPtr<> event) override;

    /**
     * Forwards call to a wrapped stream.
     * @see Stream::flush()
     */
    void flush() override;

    /**
     * Increments subscriptions reference count.
     */
    void share();

    /**
     * Decrements subscriptions reference count.
     * @return True if reference count goes to zero, otherwise false.
     */
    bool release();

private:
    StreamPtr m_stream;
    std::uint64_t m_counter = 1;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_SHARED_STREAM_H
