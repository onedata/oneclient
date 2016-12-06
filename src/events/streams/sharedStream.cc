/**
 * @file sharedStream.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/events.h"

namespace one {
namespace client {
namespace events {

SharedStream::SharedStream(StreamPtr stream)
    : m_stream{std::move(stream)}
{
}

void SharedStream::process(EventPtr<> event)
{
    m_stream->process(std::move(event));
}

void SharedStream::flush() { m_stream->flush(); }

void SharedStream::share() { ++m_counter; }

bool SharedStream::release() { return --m_counter == 0; }

} // namespace events
} // namespace client
} // namespace one
