/**
 * @file asyncStream.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_STREAMS_ASYNC_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_ASYNC_STREAM_H

#include "stream.h"

#include "folly/executors/IOThreadPoolExecutor.h"

namespace one {
namespace client {
namespace events {

/**
 * @c AsyncStream is an event stream wrapper that processes events in a single,
 * dedicated thread managed by an IO service. Therefore, synchronization
 * mechanisms are not necessary within the @c AsyncStream.
 */
class AsyncStream : public Stream {
public:
    /**
     * Constructor.
     * @param stream A wrapped @c Stream instance.
     */
    AsyncStream(StreamPtr stream);

    /**
     * Stops IO service and joins worker thread.
     */
    ~AsyncStream() = default;

    /**
     * Forwards call to a wrapped stream managed by a single, dedicated worker
     * thread.
     * @see Stream::process(EventPtr<> event)
     */
    void process(EventPtr<> event) override;

    /**
     * Forwards call to a wrapped stream managed by a single, dedicated worker
     * thread.
     * @see Stream::flush()
     */
    void flush() override;

private:
    std::shared_ptr<folly::IOThreadPoolExecutor> m_executor;
    StreamPtr m_stream;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_ASYNC_STREAM_H
