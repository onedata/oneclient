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

class SharedStream : public Stream {
public:
    SharedStream(StreamPtr stream);

    void process(EventPtr<> event) override;

    void flush() override;

    void share();

    bool release();

private:
    StreamPtr m_stream;
    std::uint64_t m_counter = 1;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_SHARED_STREAM_H
