/**
 * @file events_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "testCommon.h"
#include "scheduler.h"
#include "scheduler_mock.h"
#include "eventCommunicator_mock.h"

#include "events/eventStream.h"
#include "events/eventCommunicator.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"
#include "events/aggregators/nullAggregator.h"
#include "events/aggregators/fileIdAggregator.h"
#include "messages/server/readEventSubscription.h"

#include <boost/icl/interval_set.hpp>

#include <list>
#include <memory>
#include <chrono>
#include <thread>
#include <cstdint>

using namespace ::testing;
using namespace one;
using namespace one::client;
using namespace one::client::events;
using namespace std::literals::chrono_literals;

typedef boost::icl::interval_set<off_t> Blocks;

inline boost::icl::discrete_interval<off_t> block(off_t offset, size_t size)
{
    return boost::icl::discrete_interval<off_t>::right_open(offset,
                                                            offset + size);
}

class Streams : public CommonTest {
protected:
    std::shared_ptr<MockEventCommunicator> eventCommunicator;

    void SetUp() override
    {
        CommonTest::SetUp();
        eventCommunicator = std::make_shared<MockEventCommunicator>(context);

        ON_CALL(*scheduler, schedule(_, _)).WillByDefault(Return([] {}));
    }
};

TEST(Aggregators, NullAggregatorTest)
{
    std::unique_ptr<Aggregator<ReadEvent>> aggregator =
        std::make_unique<NullAggregator<ReadEvent>>();

    const ReadEvent &aggregatedEvent =
        aggregator->aggregate(ReadEvent{"fileId1", 0, 10});
    EXPECT_EQ(ReadEvent(), aggregatedEvent);
    EXPECT_EQ(aggregatedEvent, aggregator->all());
    EXPECT_TRUE(aggregator->reset().empty());
}

TEST(Aggregators, FileIdReadEventAggregatorTest)
{
    std::unique_ptr<Aggregator<ReadEvent>> aggregator =
        std::make_unique<FileIdAggregator<ReadEvent>>();

    const ReadEvent &aggregatedEvent1 =
        aggregator->aggregate(ReadEvent{"fileId1", 0, 10});
    EXPECT_EQ(1, aggregatedEvent1.counter());
    EXPECT_EQ(10, aggregatedEvent1.size());
    EXPECT_EQ(aggregatedEvent1, aggregator->all());

    const ReadEvent &aggregatedEvent2 =
        aggregator->aggregate(ReadEvent{"fileId1", 10, 5});
    EXPECT_EQ(2, aggregatedEvent2.counter());
    EXPECT_EQ(15, aggregatedEvent2.size());
    EXPECT_EQ(aggregatedEvent2, aggregator->all());

    const ReadEvent &aggregatedEvent3 =
        aggregator->aggregate(ReadEvent{"fileId2", 0, 5});
    EXPECT_EQ(3, aggregatedEvent3.counter());
    EXPECT_EQ(20, aggregatedEvent3.size());
    EXPECT_EQ(aggregatedEvent3, aggregator->all());

    const ReadEvent &aggregatedEvent4 =
        aggregator->aggregate(ReadEvent{"fileId3", 0, 10});
    EXPECT_EQ(4, aggregatedEvent4.counter());
    EXPECT_EQ(30, aggregatedEvent4.size());
    EXPECT_EQ(aggregatedEvent4, aggregator->all());

    std::list<ReadEvent> aggregatedEvents = aggregator->reset();

    EXPECT_EQ(3, aggregatedEvents.size());
    EXPECT_EQ("fileId1", aggregatedEvents.front().fileId());
    EXPECT_EQ(2, aggregatedEvents.front().counter());
    EXPECT_EQ(15, aggregatedEvents.front().size());
    EXPECT_TRUE(Blocks{block(0, 15)} == aggregatedEvents.front().blocks());
    aggregatedEvents.pop_front();

    EXPECT_EQ("fileId2", aggregatedEvents.front().fileId());
    EXPECT_EQ(1, aggregatedEvents.front().counter());
    EXPECT_EQ(5, aggregatedEvents.front().size());
    EXPECT_TRUE(Blocks{block(0, 5)} == aggregatedEvents.front().blocks());
    aggregatedEvents.pop_front();

    EXPECT_EQ("fileId3", aggregatedEvents.front().fileId());
    EXPECT_EQ(1, aggregatedEvents.front().counter());
    EXPECT_EQ(10, aggregatedEvents.front().size());
    EXPECT_TRUE(Blocks{block(0, 10)} == aggregatedEvents.front().blocks());
    aggregatedEvents.pop_front();

    EXPECT_EQ(ReadEvent(), aggregator->all());
    EXPECT_EQ(aggregatedEvents, aggregator->reset());
}

TEST(Aggregators, FileIdWriteEventAggregatorTest)
{
    std::unique_ptr<Aggregator<WriteEvent>> aggregator =
        std::make_unique<FileIdAggregator<WriteEvent>>();

    const WriteEvent &aggregatedEvent1 =
        aggregator->aggregate(WriteEvent{"fileId1", 0, 10, 10});
    EXPECT_EQ(1, aggregatedEvent1.counter());
    EXPECT_EQ(10, aggregatedEvent1.size());
    EXPECT_EQ(10, aggregatedEvent1.fileSize());
    EXPECT_EQ(aggregatedEvent1, aggregator->all());

    const WriteEvent &aggregatedEvent2 =
        aggregator->aggregate(WriteEvent{"fileId1", 10, 5, 15});
    EXPECT_EQ(2, aggregatedEvent2.counter());
    EXPECT_EQ(15, aggregatedEvent2.size());
    EXPECT_EQ(15, aggregatedEvent2.fileSize());
    EXPECT_EQ(aggregatedEvent2, aggregator->all());

    const WriteEvent &aggregatedEvent3 =
        aggregator->aggregate(TruncateEvent{"fileId1", 10});
    EXPECT_EQ(3, aggregatedEvent3.counter());
    EXPECT_EQ(15, aggregatedEvent3.size());
    EXPECT_EQ(10, aggregatedEvent3.fileSize());
    EXPECT_EQ(aggregatedEvent3, aggregator->all());

    const WriteEvent &aggregatedEvent4 =
        aggregator->aggregate(WriteEvent{"fileId2", 0, 5, 5});
    EXPECT_EQ(4, aggregatedEvent4.counter());
    EXPECT_EQ(20, aggregatedEvent4.size());
    EXPECT_EQ(aggregatedEvent4, aggregator->all());

    const WriteEvent &aggregatedEvent5 =
        aggregator->aggregate(WriteEvent{"fileId2", 0, 10, 10});
    EXPECT_EQ(5, aggregatedEvent5.counter());
    EXPECT_EQ(30, aggregatedEvent5.size());
    EXPECT_EQ(aggregatedEvent5, aggregator->all());

    std::list<WriteEvent> aggregatedEvents = aggregator->reset();

    EXPECT_EQ(2, aggregatedEvents.size());
    EXPECT_EQ("fileId1", aggregatedEvents.front().fileId());
    EXPECT_EQ(3, aggregatedEvents.front().counter());
    EXPECT_EQ(15, aggregatedEvents.front().size());
    EXPECT_EQ(10, aggregatedEvents.front().fileSize());
    EXPECT_TRUE(Blocks{block(0, 10)} == aggregatedEvents.front().blocks());
    aggregatedEvents.pop_front();

    EXPECT_EQ("fileId2", aggregatedEvents.front().fileId());
    EXPECT_EQ(2, aggregatedEvents.front().counter());
    EXPECT_EQ(15, aggregatedEvents.front().size());
    EXPECT_EQ(10, aggregatedEvents.front().fileSize());
    EXPECT_TRUE(Blocks{block(0, 10)} == aggregatedEvents.front().blocks());
    aggregatedEvents.pop_front();

    EXPECT_EQ(WriteEvent(), aggregator->all());
    EXPECT_EQ(aggregatedEvents, aggregator->reset());
}

TEST_F(Streams, CounterThresholdEmission)
{
    std::unique_ptr<EventStream<ReadEvent, ReadEventSubscription>> stream =
        std::make_unique<EventStream<ReadEvent, ReadEventSubscription>>(
            context, eventCommunicator);

    EXPECT_CALL(*eventCommunicator, send(_))
        .WillOnce(Invoke([](const Event &event) {
            const ReadEvent &readEvent = static_cast<const ReadEvent &>(event);
            EXPECT_EQ("fileId", readEvent.fileId());
            EXPECT_EQ(10, readEvent.counter());
            EXPECT_EQ(10, readEvent.size());
            EXPECT_TRUE(Blocks{block(0, 10)} == readEvent.blocks());
        }));

    ReadEventSubscription subscription{1, 10, 10s, 100};

    stream->push(ReadEvent{"fileId", 0, 100});

    EXPECT_EQ(1, stream->addSubscription(subscription));

    for (int i = 0; i < 10; ++i)
        stream->push(ReadEvent{"fileId", i, 1});

    stream->removeSubscription(subscription);

    stream->push(ReadEvent{"fileId", 0, 100});
}

TEST_F(Streams, TimeThresholdEmission)
{
    auto context = std::make_shared<Context>();
    context->setScheduler(std::make_shared<Scheduler>(2));

    std::unique_ptr<EventStream<ReadEvent, ReadEventSubscription>> stream =
        std::make_unique<EventStream<ReadEvent, ReadEventSubscription>>(
            context, eventCommunicator);

    EXPECT_CALL(*eventCommunicator, send(_))
        .WillOnce(Invoke([](const Event &event) {
            const ReadEvent &readEvent = static_cast<const ReadEvent &>(event);
            EXPECT_EQ("fileId", readEvent.fileId());
            EXPECT_EQ(5, readEvent.counter());
            EXPECT_EQ(5, readEvent.size());
            EXPECT_TRUE(Blocks{block(0, 5)} == readEvent.blocks());
        }));

    ReadEventSubscription subscription{1, 100, 1s, 100};

    EXPECT_EQ(1, stream->addSubscription(subscription));

    for (int i = 0; i < 5; ++i)
        stream->push(ReadEvent{"fileId", i, 1});

    std::this_thread::sleep_for(2s);

    stream->removeSubscription(subscription);
}

TEST_F(Streams, SizeThresholdEmission)
{
    std::unique_ptr<EventStream<ReadEvent, ReadEventSubscription>> stream =
        std::make_unique<EventStream<ReadEvent, ReadEventSubscription>>(
            context, eventCommunicator);

    EXPECT_CALL(*eventCommunicator, send(_))
        .WillOnce(Invoke([](const Event &event) {
            const ReadEvent &readEvent = static_cast<const ReadEvent &>(event);
            EXPECT_EQ("fileId", readEvent.fileId());
            EXPECT_EQ(10, readEvent.counter());
            EXPECT_EQ(1000, readEvent.size());
            EXPECT_TRUE(Blocks{block(0, 1000)} == readEvent.blocks());
        }));

    ReadEventSubscription subscription{1, 100, 10s, 1000};

    stream->push(ReadEvent{"fileId", 0, 100});

    EXPECT_EQ(1, stream->addSubscription(subscription));

    for (int i = 0; i < 10; ++i)
        stream->push(ReadEvent{"fileId", i * 100, 100});

    stream->removeSubscription(subscription);

    for (int i = 0; i < 100; ++i)
        stream->push(ReadEvent{"fileId", i, 1});
}
