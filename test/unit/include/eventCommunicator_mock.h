/**
 * @file eventCommunicator_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENT_COMMUNICATOR_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENT_COMMUNICATOR_MOCK_H

#include "events/eventCommunicator.h"

#include <gmock/gmock.h>

namespace one {
namespace client {
class Context;
namespace events {
class Event;
}
}
}

class MockEventCommunicator : public one::client::events::EventCommunicator {
public:
    MockEventCommunicator(std::shared_ptr<one::client::Context> ctx)
        : EventCommunicator{std::move(ctx)}
    {
    }

    MOCK_CONST_METHOD1(send, void(const one::client::events::Event &));
};

#endif // ONECLIENT_TEST_UNIT_EVENT_COMMUNICATOR_MOCK_H
