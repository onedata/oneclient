/**
 * @file localStream.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_STREAMS_LOCAL_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_LOCAL_STREAM_H

#include "stream.h"

namespace one {
namespace client {
namespace events {

class LocalStream : public Stream {
public:
    LocalStream(
        AggregatorPtr aggregator, EmitterPtr emitter, HandlerPtr handler);

    void process(ConstEventPtr event) override;

    void flush() override;

private:
    AggregatorPtr m_aggregator;
    EmitterPtr m_emitter;
    HandlerPtr m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_LOCAL_STREAM_H
