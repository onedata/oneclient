/**
 * @file event_buffers_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "events/buffers/eventBufferMap.h"
#include "events/buffers/voidEventBuffer.h"

#include <algorithm>
#include <functional>
#include <vector>

using namespace one::client::events;

template <class EventT> class EventBufferTest : public ::testing::Test {
public:
    using EventPtr = std::unique_ptr<EventT>;
    using EventHandler = typename EventBuffer<EventT>::EventHandler;

    EventBufferTest()
        : onClearHandler{
              [this](EventPtr event) { events.push_back(std::move(*event)); }}
    {
    }

protected:
    std::vector<EventT> events;
    EventHandler onClearHandler;
    std::unique_ptr<EventBuffer<EventT>> buffer;
};

template <class EventT>
class VoidEventBufferTest : public EventBufferTest<EventT> {
public:
    VoidEventBufferTest()
    {
        this->buffer = std::make_unique<VoidEventBuffer<EventT>>();
        this->buffer->setOnClearHandler(this->onClearHandler);
    }
};

TYPED_TEST_CASE(VoidEventBufferTest, TestEventTypes);

template <class EventT>
class EventBufferMapTest : public EventBufferTest<EventT> {
public:
    EventBufferMapTest()
    {
        this->buffer = std::make_unique<EventBufferMap<EventT>>();
        this->buffer->setOnClearHandler(this->onClearHandler);
    }
};

TYPED_TEST_CASE(EventBufferMapTest, TestEventTypes);

TYPED_TEST(VoidEventBufferTest, pushShouldNotAggregateEvents)
{
    std::vector<std::string> fileUuids{"1", "2", "3", "4", "5"};
    for (int i = 0; i < 10; ++i)
        for (const auto &fileUuid : fileUuids)
            this->buffer->push(std::make_unique<TypeParam>(fileUuid));
    this->buffer->clear();

    EXPECT_TRUE(this->events.empty());
}

TYPED_TEST(VoidEventBufferTest, clearShouldNotCallOnClearHandler)
{
    bool called = false;
    this->buffer->setOnClearHandler([&](auto) { called = true; });
    this->buffer->clear();

    EXPECT_FALSE(called);
}

TYPED_TEST(EventBufferMapTest, pushShouldAggregateEventsByFileUuid)
{
    std::vector<std::string> fileUuids{"1", "2", "3", "4", "5"};
    for (int i = 0; i < 10; ++i)
        for (const auto &fileUuid : fileUuids)
            this->buffer->push(std::make_unique<TypeParam>(fileUuid));
    this->buffer->clear();

    sort(this->events.begin(), this->events.end());
    EXPECT_EQ(fileUuids.size(), this->events.size());
    for (unsigned int i = 0; i < this->events.size(); ++i) {
        EXPECT_EQ(fileUuids[i], this->events[i].fileUuid());
        EXPECT_EQ(10, this->events[i].counter());
    }
}

TYPED_TEST(
    EventBufferMapTest, clearShouldCallOnClearHandlerForEachAggregatedEvent)
{
    std::vector<std::string> fileUuids{"1", "2", "3", "4", "5"};
    int counter = 0;
    this->buffer->setOnClearHandler([&](auto) { ++counter; });

    for (const auto &fileUuid : fileUuids)
        this->buffer->push(std::make_unique<TypeParam>(fileUuid));
    this->buffer->clear();

    EXPECT_EQ(fileUuids.size(), counter);
}

TYPED_TEST(EventBufferMapTest, EventBufferMapShouldClearOnDestruction)
{
    std::vector<std::string> fileUuids{"1", "2", "3", "4", "5"};

    {
        auto tempBuffer = std::move(this->buffer);
        for (const auto &fileUuid : fileUuids)
            tempBuffer->push(std::make_unique<TypeParam>(fileUuid));
    }

    EXPECT_EQ(fileUuids.size(), this->events.size());
}
