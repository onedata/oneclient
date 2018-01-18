/**
 * @file asyncStream.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "asyncStream.h"
#include "communication/etls/utils.h"
#include "events/types/event.h"
#include "logging.h"

namespace one {
namespace client {
namespace events {

AsyncStream::AsyncStream(StreamPtr stream)
    : m_ioService{1}
    , m_idleWork{asio::make_work(m_ioService)}
    , m_worker{[=] {
        communication::etls::utils::nameThread("AsyncStream");
        m_ioService.run();
    }}
    , m_stream{std::move(stream)}
{
}

AsyncStream::~AsyncStream()
{
    m_ioService.stop();
    m_worker.join();
}

void AsyncStream::process(EventPtr<> event)
{
    LOG_FCALL();

    asio::post(m_ioService, [ this, event = std::move(event) ]() mutable {
        m_stream->process(std::move(event));
    });
}

void AsyncStream::flush()
{
    LOG_FCALL();

    asio::post(m_ioService, [this] { m_stream->flush(); });
}

} // namespace events
} // namespace client
} // namespace one
