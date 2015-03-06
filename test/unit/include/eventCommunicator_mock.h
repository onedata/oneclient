/**
 * @file eventCommunicator_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef EVENT_COMMUNICATOR_MOCK_H
#define EVENT_COMMUNICATOR_MOCK_H

#include "context.h"
#include "events/types/event.h"
#include "events/eventCommunicator.h"

#include <gmock/gmock.h>

class MockEventCommunicator : public one::client::events::EventCommunicator {
public:
    MockEventCommunicator(std::shared_ptr<one::client::Context> context)
        : EventCommunicator{std::move(context)}
    {
    }

    MOCK_CONST_METHOD1(send, void(const one::client::events::Event &event));
};

#endif // EVENT_COMMUNICATOR_MOCK_H
