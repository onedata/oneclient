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

class Stream {
public:
    virtual ~Stream() = default;

    virtual void process(ConstEventPtr event) = 0;

    virtual void flush() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_STREAM_H
