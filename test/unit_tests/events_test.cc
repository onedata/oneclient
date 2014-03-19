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
#include "fuse_messages.pb.h"

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

// checks simple stream with single EventFilter
TEST(EventFilter, SimpleFilter) {
	// given
	shared_ptr<Event> mkdirEvent = Event::createMkdirEvent("user1", "file1");
	shared_ptr<Event> writeEvent = Event::createWriteEvent("user1", "file2", 100);
	EventFilter filter("type", "mkdir_event");

	// what
	shared_ptr<Event> resEvent = filter.processEvent(writeEvent);
	ASSERT_FALSE((bool) resEvent);

	resEvent = filter.processEvent(mkdirEvent);
	ASSERT_TRUE((bool) resEvent);
	ASSERT_EQ(string("user1"), resEvent->getProperty("userId", string("")));
	ASSERT_EQ(string("file1"), resEvent->getProperty("fileId", string("")));
}

// checks simple stream with single EventAggregator
TEST(EventAggregatorTest, SimpleAggregation) {
	// given
	shared_ptr<Event> mkdirEvent = Event::createMkdirEvent("user1", "file1");
	shared_ptr<Event> writeEvent = Event::createWriteEvent("user1", "file1", 100);
	EventAggregator aggregator(5);

	// what
	for(int i=0; i<4; ++i){
		shared_ptr<Event> res = aggregator.processEvent(mkdirEvent);
		ASSERT_FALSE((bool) res);
	}

	// then
	shared_ptr<Event> res = aggregator.processEvent(writeEvent);
	ASSERT_TRUE((bool) res);

	// with aggregator configured that way there should be just one property
	ASSERT_EQ(1, res->properties.size());
	ASSERT_EQ(5L, res->getProperty<long long>("count", -1L));

	for(int i=0; i<4; ++i){
		shared_ptr<Event> res = aggregator.processEvent(mkdirEvent);
		ASSERT_FALSE((bool) res);
	}

	res = aggregator.processEvent(writeEvent);
	ASSERT_TRUE((bool) res);
	ASSERT_EQ(1, res->properties.size());
	ASSERT_EQ(5, res->getProperty<long long>("count", -1L));
}

TEST(EventAggregatorTest, AggregationByOneField) {
	// given
	shared_ptr<Event> mkdirEvent = Event::createMkdirEvent("user1", "file1");
	shared_ptr<Event> writeEvent = Event::createWriteEvent("user1", "file1", 100);
	EventAggregator aggregator("type", 5);

	// what
	for(int i=0; i<4; ++i){
		shared_ptr<Event> res = aggregator.processEvent(mkdirEvent);
		ASSERT_FALSE((bool) res);	
	}
	shared_ptr<Event> res = aggregator.processEvent(writeEvent);
	ASSERT_FALSE((bool) res);

	res = aggregator.processEvent(mkdirEvent);
	ASSERT_TRUE((bool) res);
	ASSERT_EQ(2, res->properties.size());
	ASSERT_EQ(5, res->getProperty<long long>("count", -1L));
	ASSERT_EQ("mkdir_event", res->getProperty("type", string("")));
	
	// we are sending just 3 writeEvents because one has already been sent
	for(int i=0; i<3; ++i){
		shared_ptr<Event> res = aggregator.processEvent(writeEvent);
		ASSERT_FALSE((bool) res);	
	}

	res = aggregator.processEvent(mkdirEvent);
	ASSERT_FALSE((bool) res);

	res = aggregator.processEvent(writeEvent);
	ASSERT_TRUE((bool) res);
	ASSERT_EQ(2, res->properties.size());
	ASSERT_EQ(5, res->getProperty<long long>("count", -1L));
	ASSERT_EQ("write_event", res->getProperty("type", string("")));
}

// checks event filter composed with event aggregator
TEST(EventAggregatorTest, FilterAndAggregation) {
	shared_ptr<Event> file1Event = Event::createMkdirEvent("user1", "file1");
	shared_ptr<Event> file2Event = Event::createMkdirEvent("user1", "file2");
	shared_ptr<Event> writeEvent = Event::createWriteEvent("user1", "file1", 100);
	shared_ptr<Event> writeEvent2 = Event::createWriteEvent("user1", "file2", 100);
	shared_ptr<IEventStream> filter(new EventFilter("type", "mkdir_event"));
	shared_ptr<IEventStream> aggregator(new EventAggregator(filter, "fileId", 5));

	for(int i=0; i<4; ++i){
		shared_ptr<Event> res = aggregator->processEvent(file1Event);
		ASSERT_FALSE((bool) res);
	}

	shared_ptr<Event> res = aggregator->processEvent(file2Event);
	ASSERT_FALSE((bool) res);

	res = aggregator->processEvent(writeEvent);
	ASSERT_FALSE((bool) res);

	res = aggregator->processEvent(file1Event);
	ASSERT_TRUE((bool) res);
	ASSERT_EQ(2, res->properties.size());
	ASSERT_EQ(5, res->getProperty<long long>("count", -1L));
	ASSERT_EQ("file1", res->getProperty("fileId", string("")));

	for(int i=0; i<3; ++i){
		shared_ptr<Event> res = aggregator->processEvent(file2Event);
		ASSERT_FALSE((bool) res);
	}

	res = aggregator->processEvent(file2Event);
	ASSERT_TRUE((bool) res);
	ASSERT_EQ(2, res->properties.size());
	ASSERT_EQ(5, res->getProperty<long long>("count", -1L));
	ASSERT_EQ("file2", res->getProperty("fileId", string("")));

	for(int i=0; i<5; ++i){
		shared_ptr<Event> res = aggregator->processEvent(writeEvent2);
		ASSERT_FALSE((bool) res);
	}
}

TEST(EventStreamCombiner, CombineStreams) {
	shared_ptr<Event> mkdirEvent = Event::createMkdirEvent("user1", "file1");
	shared_ptr<Event> writeEvent = Event::createWriteEvent("user1", "file1", 100);
	shared_ptr<IEventStream> mkdirFilter(new EventFilter("type", "mkdir_event"));
	EventStreamCombiner combiner;
	combiner.addSubstream(mkdirFilter);

	list<shared_ptr<Event> > events = combiner.processEvent(mkdirEvent);
	ASSERT_EQ(1, events.size());

	events = combiner.processEvent(writeEvent);
	ASSERT_EQ(0, events.size());

	shared_ptr<IEventStream> writeFilter(new EventFilter("type", "write_event"));
	combiner.addSubstream(writeFilter);

	events = combiner.processEvent(writeEvent);
	ASSERT_EQ(1, events.size());

	events = combiner.processEvent(mkdirEvent);
	ASSERT_EQ(1, events.size());
}

// checks if EventStreams are created correctly from EventStreamConfig proto buff message
// proto buff messages are not easy to mock because their methods are nonvirtual. Mocking is possible but would need
// changes in code which is not worth it
TEST(IEventStream, ConstructFromConfig1) {
	using namespace veil::protocol::fuse_messages;

	// given
	//EventFilterConfig filterConfig;
	EventStreamConfig config;
	EventFilterConfig * filterConfig = config.mutable_filter_config();
	filterConfig->set_field_name("type");
	filterConfig->set_desired_value("write_event");
	
	// what
	shared_ptr<IEventStream> stream = IEventStreamFactory::fromConfig(config);

	// then
	ASSERT_TRUE((bool) stream);
	EventFilter * eventFilter = dynamic_cast<EventFilter *>(stream.get());
	ASSERT_TRUE(eventFilter != NULL);
	ASSERT_EQ("type", eventFilter->getFieldName());
	ASSERT_EQ("write_event", eventFilter->getDesiredValue());
	ASSERT_FALSE((bool) eventFilter->getWrappedStream());
}

TEST(IEventStream, ConstructFromConfig2) {
	using namespace veil::protocol::fuse_messages;

	// given
	EventStreamConfig config;
	EventAggregatorConfig * aggregatorConfig = config.mutable_aggregator_config();
	aggregatorConfig->set_field_name("userId");
	aggregatorConfig->set_threshold(15);
	EventStreamConfig * wrappedConfig = config.mutable_wrapped_config();
	EventFilterConfig * filterConfig = wrappedConfig->mutable_filter_config();
	filterConfig->set_field_name("type");
	filterConfig->set_desired_value("write_event");
	
	// what
	shared_ptr<IEventStream> stream = IEventStreamFactory::fromConfig(config);

	// then
	ASSERT_TRUE((bool) stream);
	EventAggregator * eventAggregator = dynamic_cast<EventAggregator *>(stream.get());
	ASSERT_TRUE(eventAggregator != NULL);
	ASSERT_EQ("userId", eventAggregator->getFieldName());
	ASSERT_EQ(15, eventAggregator->getThreshold());
	shared_ptr<IEventStream> wrappedStream = eventAggregator->getWrappedStream();
	ASSERT_TRUE((bool) wrappedStream);
	EventFilter * eventFilter = dynamic_cast<EventFilter *> (wrappedStream.get());
	ASSERT_TRUE(eventFilter != NULL);
	ASSERT_EQ("type", eventFilter->getFieldName());
	ASSERT_EQ("write_event", eventFilter->getDesiredValue());
	ASSERT_FALSE((bool) eventFilter->getWrappedStream());
}

TEST(IEventStream, ConstructFromConfigReturnsEmptyPointerWhenConfigIncorrect){
	using namespace veil::protocol::fuse_messages;

	// given
	EventStreamConfig config;

	// what
	shared_ptr<IEventStream> stream = IEventStreamFactory::fromConfig(config);

	//config was incorrect so we expect IEventStreamFactory::fromConfig to return empty shared_ptr
	ASSERT_FALSE((bool) stream);
}
