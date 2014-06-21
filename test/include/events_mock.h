/**
 * @file events_mock.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENTS_MOCK_H
#define EVENTS_MOCK_H

#include "events/events.h"

#include "testCommon.h"
#include "fuse_messages.pb.h"
#include "context.h"

#include <memory>
#include <list>
#include <string>

class MockEvent : public events::Event {
	
};

class MockEventCommunicator : public EventCommunicator {
public:
    MockEventCommunicator(std::shared_ptr<Context> context)
    	: EventCommunicator{std::move(context)} {}
	~MockEventCommunicator(){}

	MOCK_METHOD1(processEvent, void(boost::shared_ptr<Event>));
};

class MockEventStreamCombiner : public EventStreamCombiner{
public:
	MockEventStreamCombiner(std::shared_ptr<Context> context)
		: EventStreamCombiner{std::move(context)} {}
	~MockEventStreamCombiner(){}

	MOCK_METHOD1(pushEventToProcess, void(boost::shared_ptr<Event>));
};

class MockEventStream : public IEventStream{
public:
	MockEventStream(){}
	~MockEventStream(){}

	MOCK_METHOD1(processEvent, boost::shared_ptr<Event>(boost::shared_ptr<Event>));
	MOCK_METHOD1(actualProcessEvent, boost::shared_ptr<Event>(boost::shared_ptr<Event>));
};

#endif // EVENTS_MOCK_H
