/**
 * @file stream.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "localStream.h"
#include "events/aggregators/aggregator.h"
#include "events/emitters/emitter.h"
#include "events/handlers/handler.h"

namespace one {
namespace client {
namespace events {

LocalStream::LocalStream(
    AggregatorPtr aggregator, EmitterPtr emitter, HandlerPtr handler)
    : m_aggregator{std::move(aggregator)}
    , m_emitter{std::move(emitter)}
    , m_handler{std::move(handler)}
{
}

void LocalStream::process(ConstEventPtr event)
{
    m_aggregator->process(event);
    m_emitter->process(event);
    if (m_emitter->ready()) {
        m_handler->process(m_aggregator->flush());
        m_emitter->reset();
    }
}

void LocalStream::flush()
{
    m_handler->process(m_aggregator->flush());
    m_emitter->reset();
}

} // namespace events
} // namespace client
} // namespace one
