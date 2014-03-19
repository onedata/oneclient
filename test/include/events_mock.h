/**
 * @file events_mock.h
 * @author Michal Sitko
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENTS_MOCK_H
#define EVENTS_MOCK_H

#include "events.h"
#include "testCommon.h"
#include <list>
#include <string>
#include "fuse_messages.pb.h"

class MockEvent : public Event {
	
};

class MockEventCommunicator : public EventCommunicator {
public:
	MockEventCommunicator(){}
	~MockEventCommunicator(){}

	MOCK_METHOD1(processEvent, void(boost::shared_ptr<Event>));
};

#endif // EVENTS_MOCK_H