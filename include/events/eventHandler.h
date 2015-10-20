/**
 * @file eventHandler.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EVENT_HANDLER_H
#define ONECLIENT_EVENTS_EVENT_HANDLER_H

#include "events/buffers/voidEventBuffer.h"

#include <functional>
#include <unordered_map>

namespace one {
namespace client {
namespace events {

/**
 * @c EventHandler is responsible for handling events. It executes registered
 * handlers on events stored in the @c EventBuffer or forwards events to the @c
 * LowerLayer if there is no handlers.
 */
template <class LowerLayer> class EventHandler : public LowerLayer {
public:
    using EventT = typename LowerLayer::EventT;
    using EventPtr = typename LowerLayer::EventPtr;
    using BufferPtr = std::unique_ptr<EventBuffer<EventT>>;
    using Handler = std::function<void(EventPtr)>;
    using OnTriggerCallback = std::function<void()>;

    using LowerLayer::LowerLayer;
    virtual ~EventHandler() = default;

    /**
     * Pushes an event to the @c EventBuffer.
     * @param event Event to be processed.
     */
    void process(EventPtr event);

    /**
     * Clears the @c EventBuffer and calls @c OnTriggerCallback.
     */
    void trigger();

    /**
     * Pushes an event to the @c EventBuffer and immediately after clears the @c
     * EventBuffer and calls @c OnTriggerCallback.
     * @param event Event to be processed.
     */
    void trigger(EventPtr event);

    /**
     * Overwrites default event handler which forwards events to the @c
     * LowerLayer.
     * @param handler @c Handler to be set.
     */
    void setEventHandler(Handler handler);

    /**
     * Sets the @c EventBuffer and initializes @c EventBuffer::EventHandler
     * which will execute all registered handlers or forward event to the @c
     * LowerLayer if there are no handlers.
     * @param buffer @c EventBuffer instance.
     */
    void setEventBuffer(BufferPtr buffer);

    /**
     * Sets callback which will be called after @c trigger method execution.
     * @param callback Callback to be set.
     */
    void setOnTriggerCallback(OnTriggerCallback callback);

private:
    BufferPtr m_buffer{std::make_unique<VoidEventBuffer<EventT>>()};
    Handler m_handler = [](auto) {};
    OnTriggerCallback m_onTriggerCallback;
};

template <class LowerLayer>
void EventHandler<LowerLayer>::process(EventPtr event)
{
    m_buffer->push(std::move(event));
}

template <class LowerLayer> void EventHandler<LowerLayer>::trigger()
{
    m_buffer->clear();
    m_onTriggerCallback();
}

template <class LowerLayer>
void EventHandler<LowerLayer>::trigger(EventPtr event)
{
    process(std::move(event));
    trigger();
}

template <class LowerLayer>
void EventHandler<LowerLayer>::setEventHandler(Handler handler)
{
    m_handler = std::move(handler);
}

template <class LowerLayer>
void EventHandler<LowerLayer>::setEventBuffer(BufferPtr buffer)
{
    m_buffer = std::move(buffer);
    m_buffer->setOnClearHandler(
        [this](EventPtr event) { m_handler(std::move(event)); });
}

template <class LowerLayer>
void EventHandler<LowerLayer>::setOnTriggerCallback(OnTriggerCallback callback)
{
    m_onTriggerCallback = std::move(callback);
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_HANDLER_H
