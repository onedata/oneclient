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

class MockEventConfiguration : public EventConfiguration {

public:
	MockEventConfiguration() : EventConfiguration() {}
	~MockEventConfiguration() {}

	MOCK_METHOD0(getSubscriptions, list<EventSubscription>());
};

class MockEvent : public Event {
	
};

#endif // EVENTS_MOCK_H