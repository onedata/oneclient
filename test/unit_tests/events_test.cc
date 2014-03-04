/**
 * @file events_test.cc
 * @author Michal Sitko
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "events.h"
#include "events_mock.h"
#include "boost/shared_ptr.hpp"

using namespace boost;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class EventsTest 
    : public ::testing::Test
{
public:

    virtual void SetUp() {
        
    }

    virtual void TearDown() {
        
    }
};

/*TEST(EventProcessorTest, SendsOnlyWhenConfigured) {
	// given
    MkdirEvent event("user1", "file1");
    EventProcessor eventProcessor;

    // what
    string res = eventProcessor.processEvent(event);

    // then
    ASSERT_TRUE(res.empty());

    // given
	boost:shared_ptr<MockEventConfiguration> emptyMockEventConfiguration (new MockEventConfiguration);
    list<EventSubscription> subscriptions;
    EXPECT_CALL(*emptyMockEventConfiguration, getSubscriptions()).Times(1).WillRepeatedly(Return(subscriptions));
    eventProcessor.setConfiguration(emptyMockEventConfiguration);

    // what
    res = eventProcessor.processEvent(event);

    // then
    ASSERT_TRUE(res.empty());

    //given
	boost:shared_ptr<MockEventConfiguration> nonEmptyMockEventConfiguration (new MockEventConfiguration);
    list<EventSubscription> subscriptions2;
    EventSubscription subscription(1);
    subscriptions2.push_back(subscription);
    EXPECT_CALL(*nonEmptyMockEventConfiguration, getSubscriptions()).Times(1).WillRepeatedly(Return(subscriptions2));
    eventProcessor.setConfiguration(nonEmptyMockEventConfiguration);

    // what
    res = eventProcessor.processEvent(event);

    // then
    ASSERT_FALSE(res.empty());
}

TEST(EventProcessorTest, SendsOnlyWhenThresholdExceeded) {
	// given
    boost:shared_ptr<MockEventConfiguration> mockEventConfiguration (new MockEventConfiguration);
    list<EventSubscription> subscriptions;
    EventSubscription subscription (3);
    subscriptions.push_back(subscription);
    EXPECT_CALL(*mockEventConfiguration, getSubscriptions()).Times(6).WillRepeatedly(Return(subscriptions));
    MkdirEvent event("user1", "file1");
    EventProcessor eventProcessor;

    // what
    string res = eventProcessor.processEvent(event);

    // then
    ASSERT_TRUE(res.empty());
    res = eventProcessor.processEvent(event);
    ASSERT_TRUE(res.empty());
    res = eventProcessor.processEvent(event);
    ASSERT_FALSE(res.empty());

    res = eventProcessor.processEvent(event);
    ASSERT_TRUE(res.empty());
    res = eventProcessor.processEvent(event);
    ASSERT_TRUE(res.empty());
    res = eventProcessor.processEvent(event);
    ASSERT_FALSE(res.empty());
}

TEST(EventProcessorTest, AggregationByFileId) {
	// given
    shared_ptr<MockEventConfiguration> mockEventConfiguration (new MockEventConfiguration);
    list<EventSubscription> subscriptions;
    EventSubscription subscription ("fileId", 2);
    subscriptions.push_back(subscription);
    EXPECT_CALL(*mockEventConfiguration, getSubscriptions()).Times(6).WillRepeatedly(Return(subscriptions));
    MkdirEvent event1("user1", "file1");
    MkdirEvent event2("user1", "file2");
    EventProcessor eventProcessor;

    // what
    string res1 = eventProcessor.processEvent(event1);
    string res2 = eventProcessor.processEvent(event2);
    string res3 = eventProcessor.processEvent(event1);
    string res4 = eventProcessor.processEvent(event2);
    string res5 = eventProcessor.processEvent(event1);
    string res6 = eventProcessor.processEvent(event1);

    // then
    ASSERT_TRUE(res1.empty());
    ASSERT_TRUE(res2.empty());
    ASSERT_FALSE(res3.empty());
    ASSERT_FALSE(res4.empty());
    ASSERT_TRUE(res5.empty());
    ASSERT_FALSE(res6.empty());
}*/

TEST(EventFilter, SimpleFilter) {
	// given
	shared_ptr<Event> mkdirEvent = Event::createMkdirEvent("user1", "file1");
	shared_ptr<Event> writeEvent = Event::createWriteEvent("user1", "file1", 100);
	EventFilter filter("type", "mkdir_event");

	// what
	shared_ptr<Event> resEvent = filter.processEvent(writeEvent);
	ASSERT_EQ(0, resEvent.use_count());

	shared_ptr<Event> resEvent2 = filter.processEvent(mkdirEvent);
	//shared_ptr<Event> resEvent2 = mkdirEvent;
	ASSERT_EQ(1, resEvent2.use_count());
	//ASSERT_EQ("user1", resEvent->properties["userId"]);
	//ASSERT_EQ("file1", resEvent->properties["fileId"]);
}

/*TEST(EventAggregatorTest, SimpleAggregation) {
	// given
	shared_ptr<Event> mkdirEvent = Event::createMkdirEvent("user1", "file1");
	shared_ptr<Event> writeEvent = Event::createWriteEvent("user1", "file1", 100);
	EventAggregator aggregator(5);

	// what
	for(int i=0; i<4; ++i){
		shared_ptr<Event> res = aggregator.processEvent(mkdirEvent);
		ASSERT_EQ(0, event.use_count());
	}

	// then
	shared_ptr<Event> res = aggregator.processEvent(writeEvent);
	ASSERT_EQ(1, res.use_count());

	// with aggregator configured that way there should be just one property
	ASSERT_EQ(1, res->properties.size());
	ASSERT_EQ(5, res->properties["multiplicity"]);

	for(int i=0; i<4; ++i){
		shared_ptr<Event> res = aggregator.processEvent(mkdirEvent);
		ASSERT_EQ(0, event.use_count());
	}

	shared_ptr<Event> res2 = aggregator.processEvent(writeEvent);
	ASSERT_EQ(1, res2.use_count());
	ASSERT_EQ(1, res->properties.size());
	ASSERT_EQ(5, res->properties["multiplicity"]);
}

TEST(EventAggregatorTest, AggregationByOneField) {
	// given
	shared_ptr<Event> mdkirEvent = Event::createMkdirEvent("user1", "file1");
	shared_ptr<Event> writeEvent = Event:createWriteEvent("user1", "file1", 100);
	EventAggregator aggregator("type", 5);

	// what
	for(int i=0; i<4; ++i){
		shared_ptr<Event> res = aggregator.processEvent(mkdirEvent);
		ASSERT_EQ(0, res.use_count());	
	}
	shared_ptr<Event> res = aggregator.processEvent(writeEvent);
	ASSERT_EQ(0, res.use_count());

	res = aggregator.processEvent(mkdirEvent);
	ASSERT_EQ(1, res.use_count());
	ASSERT_EQ(2, res->properties.size());
	ASSERT_EQ(5, res->properties["multiplicity"]);
	ASSERT_EQ("mkdir_event", res->properties["type"]);
	
	// we are sending just 3 writeEvents because one has already been send
	for(int i=0; i<3; ++i){
		shared_ptr<Event> res = aggregator.processEvent(writeEvent);
		ASSERT_EQ(0, res.use_count());	
	}

	res = aggregator.processEvent(mkdirEvent);
	ASSERT_EQ(0, res.use_count());

	res = aggregator.processEvent(writeEvent);
	ASSERT_EQ(1, res.use_count());
	ASSERT_EQ(2, res->properties.size());
	ASSERT_EQ(5, res->properties["multiplicity"]);
	ASSERT_EQ("write_event", res->properties["type"]);
}

TEST(EventAggregatorTest, AggregationByTwoFields) {
	// given
	shared_ptr<Event> file1Event = Event::createMkdirEvent("user1", "file1");
	shared_ptr<Event> file2Event = Event::createMkdirEvent("user1", "file2");
	EventAggregator("type", "mkdir_event");
}*/
