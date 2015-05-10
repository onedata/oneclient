/**
 * @file eventStream_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENT_STREAM_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENT_STREAM_MOCK_H

#include "context.h"
#include "events/eventStream.h"
#include "events/eventCommunicator.h"

#include <gmock/gmock.h>

#include <memory>

template <class EventType>
class MockEventStream : public one::client::events::EventStream<EventType> {
public:
    MockEventStream(std::weak_ptr<one::client::Context> context,
        std::shared_ptr<one::client::events::EventCommunicator> communicator)
        : one::client::events::EventStream<EventType>{
              std::move(context), std::move(communicator)}
    {
    }

    virtual void pushAsync(EventType event) override
    {
        this->push(std::move(event));
    }

    virtual uint64_t addSubscriptionAsync(
        typename EventType::Subscription subscription) override
    {
        uint64_t id = subscription.id();
        this->addSubscription(std::move(subscription));
        return id;
    }

    virtual void removeSubscriptionAsync(
        typename EventType::Subscription subscription) override
    {
        this->removeSubscription(std::move(subscription));
    }

    void periodicEmission() { this->emit(); }
};

#endif // ONECLIENT_TEST_UNIT_EVENT_STREAM_MOCK_H
