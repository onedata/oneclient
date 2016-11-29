/**
 * @file counterEmitter.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "counterEmitter.h"

namespace one {
namespace client {
namespace events {

CounterEmitter::CounterEmitter(
    std::int64_t threshold, std::unique_ptr<Emitter> emitter)
    : m_threshold{threshold}
    , m_emitter{std::move(emitter)}
{
}

void CounterEmitter::process(ConstEventPtr event)
{
    ++m_counter;
    m_emitter->process(std::move(event));
}

bool CounterEmitter::ready()
{
    return (m_counter >= m_threshold) || m_emitter->ready();
}

void CounterEmitter::reset()
{
    m_counter = 0;
    m_emitter->reset();
}

} // namespace events
} // namespace client
} // namespace one
