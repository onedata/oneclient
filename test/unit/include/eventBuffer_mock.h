/**
 * @file eventBuffer_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENT_BUFFER_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENT_BUFFER_MOCK_H

#include "events/buffers/eventBuffer.h"

#include <gmock/gmock.h>

template <class EventT>
class MockEventBuffer : public one::client::events::EventBuffer<EventT> {
public:
    using EventPtr =
        typename one::client::events::EventBuffer<EventT>::EventPtr;
    using EventHandler =
        typename one::client::events::EventBuffer<EventT>::EventHandler;

    void push(EventPtr event) override { push(*event); }

    MOCK_METHOD1_T(push, void(const EventT &));
    MOCK_METHOD0_T(clear, void());
    MOCK_METHOD1_T(setOnClearHandler, void(EventHandler));
};

#endif // ONECLIENT_TEST_UNIT_EVENT_BUFFER_MOCK_H
