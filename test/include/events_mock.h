/**
 * @file events_mock.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENTS_MOCK_H
#define EVENTS_MOCK_H


#include "events/events.h"

#include "context.h"
#include "fuse_messages.pb.h"

#include <gmock/gmock.h>

#include <memory>
#include <list>
#include <string>

class MockEvent: public one::client::events::Event
{
};

class MockEventCommunicator: public one::client::events::EventCommunicator
{
public:
    MockEventCommunicator(std::shared_ptr<one::client::Context> context)
        : EventCommunicator{std::move(context)}
    {
    }

    MOCK_METHOD1(processEvent, void(std::shared_ptr<one::client::events::Event>));
};

class MockEventStreamCombiner: public one::client::events::EventStreamCombiner
{
public:
    MockEventStreamCombiner(std::shared_ptr<one::client::Context> context)
        : EventStreamCombiner{std::move(context)}
    {
    }

    MOCK_METHOD1(pushEventToProcess, void(std::shared_ptr<one::client::events::Event>));
};

class MockEventStream: public one::client::events::IEventStream
{
public:
    MOCK_METHOD1(processEvent, std::shared_ptr<one::client::events::Event>(std::shared_ptr<one::client::events::Event>));
    MOCK_METHOD1(actualProcessEvent, std::shared_ptr<one::client::events::Event>(std::shared_ptr<one::client::events::Event>));
};


#endif // EVENTS_MOCK_H
