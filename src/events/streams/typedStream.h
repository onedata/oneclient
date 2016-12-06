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

namespace one {
namespace client {
namespace events {

template <class T> class TypedStream : public Stream {
public:
    TypedStream(std::unique_ptr<Aggregator<T>> aggregator,
        EmitterPtr<T> emitter, std::unique_ptr<Handler<T>> handler);

    void process(EventPtr<> event) override;

    void flush() override;

private:
    std::unique_ptr<Aggregator<T>> m_aggregator;
    EmitterPtr<T> m_emitter;
    std::unique_ptr<Handler<T>> m_handler;
};

template <class T>
TypedStream<T>::TypedStream(std::unique_ptr<Aggregator<T>> aggregator,
    EmitterPtr<T> emitter, std::unique_ptr<Handler<T>> handler)
    : m_aggregator{std::move(aggregator)}
    , m_emitter{std::move(emitter)}
    , m_handler{std::move(handler)}
{
}

template <class T> void TypedStream<T>::process(EventPtr<> event)
{
    std::unique_ptr<T> typedEvent{dynamic_cast<T *>(event.release())};
    if (typedEvent) {
        m_aggregator->process(m_emitter->process(std::move(typedEvent)));
        if (m_emitter->ready()) {
            m_handler->process(m_aggregator->flush());
            m_emitter->reset();
        }
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
