/**
 * @file counterEmitter.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EMITTERS_COUNTER_EMITTER_H
#define ONECLIENT_EVENTS_EMITTERS_COUNTER_EMITTER_H

#include "emitter.h"
#include "falseEmitter.h"

namespace one {
namespace client {
namespace events {

/**
 * @c CounterEmitter is responsible for counting processed events and returning
 * emission ready status when number of processed events is greater or equal to
 * the specified threshold.
 */
template <class T> class CounterEmitter : public Emitter<T> {
public:
    /**
     * Constructor.
     * @param threshold A number of events which have to be processed before
     * emission.
     * @param emitter A wrapped @c Emitter instance.
     */
    CounterEmitter(std::int64_t threshold,
        EmitterPtr<T> emitter = std::make_unique<FalseEmitter<T>>());

    /**
     * Increments number of processed events and forwards call to the chained
     * emitter.
     * @see Emitter:process(EventPtr<T> event)
     */
    EventPtr<T> process(EventPtr<T> event) override;

    /**
     * Returns true if number of processed events is greater or equal to the
     * specified threshold or forwards call to the chained emitter.
     * @see Emitter::ready()
     */
    bool ready() override;

    /**
     * Sets number of processed events to zero and forwards call to the chained
     * emitter.
     * @see Emitter::reset()
     */
    void reset() override;

private:
    std::int64_t m_counter = 0;
    std::int64_t m_threshold;
    EmitterPtr<T> m_emitter;
};

template <class T>
CounterEmitter<T>::CounterEmitter(std::int64_t threshold, EmitterPtr<T> emitter)
    : m_threshold{std::move(threshold)}
    , m_emitter{std::move(emitter)}
{
}

template <class T> EventPtr<T> CounterEmitter<T>::process(EventPtr<T> event)
{
    ++m_counter;
    return m_emitter->process(std::move(event));
}

template <class T> bool CounterEmitter<T>::ready()
{
    return (m_counter >= m_threshold) || m_emitter->ready();
}

template <class T> void CounterEmitter<T>::reset()
{
    m_counter = 0;
    m_emitter->reset();
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_COUNTER_EMITTER_H
