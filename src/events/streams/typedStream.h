/**
 * @file typedStream.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_STREAMS_TYPED_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_TYPED_STREAM_H

#include "helpers/logging.h"
#include "stream.h"

#include <typeinfo>

namespace one {
namespace client {
namespace events {

/**
 * @c TypedStream is responsible for aggregation and handling of homogeneous
 * events. Events processing flow can be described as follows: first an event is
 * processed by an emitter (possibly emitters chain), then it is aggregated by
 * an aggregator, finally if an emitter declares stream ready for emission a
 * handler is called on aggregated events and emitter is reset.
 */
template <class T> class TypedStream : public Stream {
public:
    /**
     * Construtor.
     * @param aggregator An @c Aggregator instance.
     * @param emitter An @c Emitter instance.
     * @param handler An @c Handler instance.
     */
    TypedStream(AggregatorPtr<T> aggregator, EmitterPtr<T> emitter,
        HandlerPtr<T> handler);

    /**
     * Flushes the stream.
     */
    ~TypedStream();

    /**
     * Forwards the request to an emitter and then to an aggregator. Then if the
     * emitter declares stream ready for emission flushes the stream.
     */
    void process(EventPtr<> event) override;

    /**
     * Calls a handler on aggregated events and resets the emitter.
     */
    void flush() override;

private:
    AggregatorPtr<T> m_aggregator;
    EmitterPtr<T> m_emitter;
    HandlerPtr<T> m_handler;
};

template <class T>
TypedStream<T>::TypedStream(
    AggregatorPtr<T> aggregator, EmitterPtr<T> emitter, HandlerPtr<T> handler)
    : m_aggregator{std::move(aggregator)}
    , m_emitter{std::move(emitter)}
    , m_handler{std::move(handler)}
{
}

template <class T> TypedStream<T>::~TypedStream() { flush(); }

template <class T> void TypedStream<T>::process(EventPtr<> event)
{
    LOG_FCALL();

    auto rawEvent = event.release();
    std::unique_ptr<T> typedEvent{dynamic_cast<T *>(rawEvent)};
    if (typedEvent) {
        m_aggregator->process(m_emitter->process(std::move(typedEvent)));
        if (m_emitter->ready()) {
            flush();
        }
    }
    else {
        LOG(ERROR) << "Cannot process event " << rawEvent->toString()
                   << " in a typed stream '" << typeid(T).name() << "'";
    }
}

template <class T> void TypedStream<T>::flush()
{
    m_handler->process(m_aggregator->flush());
    m_emitter->reset();
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_TYPED_STREAM_H
