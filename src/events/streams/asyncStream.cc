/**
 * @file asyncStream.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "asyncStream.h"

#include "events/types/event.h"
#include "helpers/logging.h"

#include <folly/system/ThreadName.h>

namespace one {
namespace client {
namespace events {

AsyncStream::AsyncStream(StreamPtr stream)
    : m_executor{std::make_shared<folly::IOThreadPoolExecutor>(1)}
    , m_stream{std::move(stream)}
{
}

AsyncStream::~AsyncStream()
{
    m_stream.reset();
    m_executor->join();
}

void AsyncStream::process(EventPtr<> event)
{
    LOG_FCALL();

    m_executor->add([this, event = std::move(event)]() mutable {
        if (m_stream)
            m_stream->process(std::move(event));
    });
}

void AsyncStream::flush()
{
    LOG_FCALL();

    m_executor->add([this] {
        if (m_stream)
            m_stream->flush();
    });
}

} // namespace events
} // namespace client
} // namespace one
