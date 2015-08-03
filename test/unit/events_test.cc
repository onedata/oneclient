/**
 * @file events_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "communication/persistentConnection.h"
#include "communication/communicator.h"
#include "eventStream_mock.h"
#include "eventCommunicator_mock.h"
#include "events/eventStream.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"
#include "events/aggregators/nullAggregator.h"
#include "events/aggregators/fileIdAggregator.h"
#include "messages/readEventSubscription.h"
#include "scheduler_mock.h"

#include <gtest/gtest.h>
#include <boost/icl/interval_set.hpp>

#include <vector>
#include <memory>
#include <chrono>
#include <cstdint>
#include <algorithm>
#include <functional>

using namespace ::testing;
using namespace one;
using namespace one::client;
using namespace one::communication;
using namespace one::client::events;
using namespace std::literals::chrono_literals;

typedef boost::icl::interval_set<off_t> Blocks;

inline boost::icl::discrete_interval<off_t> block(off_t offset, size_t size)
{
    return boost::icl::discrete_interval<off_t>::right_open(
        offset, offset + size);
}

template <class EventType>
bool greaterFileId(const EventType &lhs, const EventType &rhs)
{
    return lhs.fileId() > rhs.fileId();
}

class AggregatorTest : public ::testing::Test {
};

class StreamTest : public ::testing::Test {
protected:
    std::shared_ptr<Context> context;
    std::shared_ptr<Communicator> communicator;
    std::shared_ptr<MockScheduler> scheduler;
    std::shared_ptr<MockEventCommunicator> eventCommunicator;
    std::shared_ptr<MockEventStream<ReadEvent>> stream;

    StreamTest()
    {
        context = std::make_shared<Context>();
        communicator = std::make_shared<Communicator>(
            1, "localhost", 80, false, communication::createConnection);
        scheduler = std::make_shared<MockScheduler>();
        context->setCommunicator(communicator);
        context->setScheduler(scheduler);
        eventCommunicator = std::make_shared<MockEventCommunicator>(context);
        ON_CALL(*scheduler, post(_))
            .WillByDefault(WithArgs<0>(
                Invoke([](const std::function<void()> &task) { task(); })));
        EXPECT_CALL(*scheduler, schedule(_, _)).WillRepeatedly(Return([] {}));
        stream = std::make_shared<MockEventStream<ReadEvent>>(
            context, eventCommunicator);
    }
};

TEST_F(AggregatorTest, nullAggregatorShouldAggregateEvents)
{
    std::unique_ptr<Aggregator<ReadEvent>> aggregator =
        std::make_unique<NullAggregator<ReadEvent>>();

    const ReadEvent &aggregatedEvent =
        aggregator->aggregate(ReadEvent{"fileId1", 0, 10});
    EXPECT_EQ(ReadEvent{}, aggregatedEvent);
    EXPECT_EQ(aggregatedEvent, aggregator->all());
    EXPECT_TRUE(aggregator->reset().empty());
}

TEST_F(AggregatorTest, fileIdAggregatorShouldAggregateReadEvents)
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
}

TEST_F(AggregatorTest, fileIdAggregatorShouldReturnListOfAggregatedReadEvents)
{
    std::unique_ptr<Aggregator<ReadEvent>> aggregator =
        std::make_unique<FileIdAggregator<ReadEvent>>();

    aggregator->aggregate(ReadEvent{"fileId1", 0, 10});
    aggregator->aggregate(ReadEvent{"fileId1", 10, 5});
    aggregator->aggregate(ReadEvent{"fileId2", 0, 5});
    aggregator->aggregate(ReadEvent{"fileId3", 0, 10});

    std::vector<ReadEvent> aggregatedEvents = aggregator->reset();
    std::sort(aggregatedEvents.begin(), aggregatedEvents.end(),
        greaterFileId<ReadEvent>);

    EXPECT_EQ(3, aggregatedEvents.size());
    EXPECT_EQ("fileId1", aggregatedEvents.back().fileId());
    EXPECT_EQ(2, aggregatedEvents.back().counter());
    EXPECT_EQ(15, aggregatedEvents.back().size());
    EXPECT_TRUE(Blocks{block(0, 15)} == aggregatedEvents.back().blocks());
    aggregatedEvents.pop_back();

    EXPECT_EQ("fileId2", aggregatedEvents.back().fileId());
    EXPECT_EQ(1, aggregatedEvents.back().counter());
    EXPECT_EQ(5, aggregatedEvents.back().size());
    EXPECT_TRUE(Blocks{block(0, 5)} == aggregatedEvents.back().blocks());
    aggregatedEvents.pop_back();

    EXPECT_EQ("fileId3", aggregatedEvents.back().fileId());
    EXPECT_EQ(1, aggregatedEvents.back().counter());
    EXPECT_EQ(10, aggregatedEvents.back().size());
    EXPECT_TRUE(Blocks{block(0, 10)} == aggregatedEvents.back().blocks());
    aggregatedEvents.pop_back();

    EXPECT_EQ(ReadEvent(), aggregator->all());
    EXPECT_EQ(aggregatedEvents, aggregator->reset());
}

TEST_F(AggregatorTest, fileIdAggregatorShouldAggregateWriteEvents)
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
}

TEST_F(AggregatorTest, fileIdAggregatorShouldReturnListOfAggregatedWriteEvents)
{
    std::unique_ptr<Aggregator<WriteEvent>> aggregator =
        std::make_unique<FileIdAggregator<WriteEvent>>();

    aggregator->aggregate(WriteEvent{"fileId1", 0, 10, 10});
    aggregator->aggregate(WriteEvent{"fileId1", 10, 5, 15});
    aggregator->aggregate(TruncateEvent{"fileId1", 10});
    aggregator->aggregate(WriteEvent{"fileId2", 0, 5, 5});
    aggregator->aggregate(WriteEvent{"fileId2", 0, 10, 10});

    std::vector<WriteEvent> aggregatedEvents = aggregator->reset();
    std::sort(aggregatedEvents.begin(), aggregatedEvents.end(),
        greaterFileId<WriteEvent>);

    EXPECT_EQ(2, aggregatedEvents.size());
    EXPECT_EQ("fileId1", aggregatedEvents.back().fileId());
    EXPECT_EQ(3, aggregatedEvents.back().counter());
    EXPECT_EQ(15, aggregatedEvents.back().size());
    EXPECT_EQ(10, aggregatedEvents.back().fileSize());
    EXPECT_TRUE(Blocks{block(0, 10)} == aggregatedEvents.back().blocks());
    aggregatedEvents.pop_back();

    EXPECT_EQ("fileId2", aggregatedEvents.back().fileId());
    EXPECT_EQ(2, aggregatedEvents.back().counter());
    EXPECT_EQ(15, aggregatedEvents.back().size());
    EXPECT_EQ(10, aggregatedEvents.back().fileSize());
    EXPECT_TRUE(Blocks{block(0, 10)} == aggregatedEvents.back().blocks());
    aggregatedEvents.pop_back();

    EXPECT_EQ(WriteEvent(), aggregator->all());
    EXPECT_EQ(aggregatedEvents, aggregator->reset());
}

TEST_F(StreamTest, eventStreamShouldEmitEventsAfterCounterThresholdExcess)
{
    EXPECT_CALL(*eventCommunicator, send(_))
        .WillOnce(Invoke([](const Event &event) {
            const ReadEvent &readEvent = static_cast<const ReadEvent &>(event);
            EXPECT_EQ("fileId", readEvent.fileId());
            EXPECT_EQ(10, readEvent.counter());
            EXPECT_EQ(10, readEvent.size());
            EXPECT_TRUE(Blocks{block(0, 10)} == readEvent.blocks());
        }));

    ReadEventSubscription subscription{1, 10, 10s, 100};

    stream->pushAsync(ReadEvent{"fileId", 0, 100});

    EXPECT_EQ(1, stream->addSubscriptionAsync(subscription));

    for (int i = 0; i < 10; ++i)
        stream->pushAsync(ReadEvent{"fileId", i, 1});

    stream->removeSubscriptionAsync(subscription);

    stream->pushAsync(ReadEvent{"fileId", 0, 100});
}

TEST_F(StreamTest, eventStreamShouldEmitEventsAfterTimeThresholdExcess)
{
    EXPECT_CALL(*eventCommunicator, send(_))
        .WillOnce(Invoke([](const Event &event) {
            const ReadEvent &readEvent = static_cast<const ReadEvent &>(event);
            EXPECT_EQ("fileId", readEvent.fileId());
            EXPECT_EQ(5, readEvent.counter());
            EXPECT_EQ(5, readEvent.size());
            EXPECT_TRUE(Blocks{block(0, 5)} == readEvent.blocks());
        }));

    ReadEventSubscription subscription{1, 100, 1s, 100};

    EXPECT_EQ(1, stream->addSubscriptionAsync(subscription));

    for (int i = 0; i < 5; ++i)
        stream->pushAsync(ReadEvent{"fileId", i, 1});

    stream->periodicEmission();

    stream->removeSubscriptionAsync(subscription);
}

TEST_F(StreamTest, eventStreamShouldEmitEventsAfterSizeThresholdExcess)
{
    EXPECT_CALL(*eventCommunicator, send(_))
        .WillOnce(Invoke([](const Event &event) {
            const ReadEvent &readEvent = static_cast<const ReadEvent &>(event);
            EXPECT_EQ("fileId", readEvent.fileId());
            EXPECT_EQ(10, readEvent.counter());
            EXPECT_EQ(1000, readEvent.size());
            EXPECT_TRUE(Blocks{block(0, 1000)} == readEvent.blocks());
        }));

    ReadEventSubscription subscription{1, 100, 10s, 1000};

    stream->pushAsync(ReadEvent{"fileId", 0, 100});

    EXPECT_EQ(1, stream->addSubscriptionAsync(subscription));

    for (int i = 0; i < 10; ++i)
        stream->pushAsync(ReadEvent{"fileId", i * 100, 100});

    stream->removeSubscriptionAsync(subscription);

    for (int i = 0; i < 100; ++i)
        stream->pushAsync(ReadEvent{"fileId", i, 1});
}
