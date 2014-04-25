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
#include <list>
#include <string>
#include "fuse_messages.pb.h"

class MockEvent : public events::Event {
	
};

class MockEventCommunicator : public EventCommunicator {
public:
	MockEventCommunicator(){}
	~MockEventCommunicator(){}

	MOCK_METHOD1(processEvent, void(boost::shared_ptr<Event>));
};

class MockEventStreamCombiner : public EventStreamCombiner{
public:
	MockEventStreamCombiner(){}
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