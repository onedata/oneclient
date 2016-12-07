/**
 * @file typedStream.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_STREAMS_TYPED_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_TYPED_STREAM_H

#include "stream.h"

#include <typeinfo>

namespace one {
namespace client {
namespace events {

template <class T> class TypedStream : public Stream {
public:
    TypedStream(AggregatorPtr<T> aggregator, EmitterPtr<T> emitter,
        HandlerPtr<T> handler);

    ~TypedStream();

    void process(EventPtr<> event) override;

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
    auto rawEvent = event.release();
    std::unique_ptr<T> typedEvent{dynamic_cast<T *>(rawEvent)};
    if (typedEvent) {
        m_aggregator->process(m_emitter->process(std::move(typedEvent)));
        if (m_emitter->ready()) {
            m_handler->process(m_aggregator->flush());
            m_emitter->reset();
        }
    }
    else {
        DLOG(ERROR) << "Cannot process event " << rawEvent->toString()
                    << " in typed stream '" << typeid(T).name() << "'";
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
