/**
 * @file events_mock.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENTS_MOCK_H
#define EVENTS_MOCK_H


#include "events/events.h"

#include "fuse_messages.pb.h"
#include "context.h"

#include <memory>
#include <list>
#include <string>

class MockEvent: public veil::client::events::Event
{
};

class MockEventCommunicator: public veil::client::events::EventCommunicator
{
public:
    MockEventCommunicator(std::shared_ptr<veil::client::Context> context)
        : EventCommunicator{std::move(context)}
    {
    }

    MOCK_METHOD1(processEvent, void(boost::shared_ptr<veil::client::events::Event>));
};

class MockEventStreamCombiner: public veil::client::events::EventStreamCombiner
{
public:
    MockEventStreamCombiner(std::shared_ptr<veil::client::Context> context)
        : EventStreamCombiner{std::move(context)}
    {
    }

    MOCK_METHOD1(pushEventToProcess, void(boost::shared_ptr<veil::client::events::Event>));
};

class MockEventStream: public veil::client::events::IEventStream
{
public:
    MOCK_METHOD1(processEvent, boost::shared_ptr<veil::client::events::Event>(boost::shared_ptr<veil::client::events::Event>));
    MOCK_METHOD1(actualProcessEvent, boost::shared_ptr<veil::client::events::Event>(boost::shared_ptr<veil::client::events::Event>));
};


#endif // EVENTS_MOCK_H
