/**
 * @file event_handler_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "eventBuffer_mock.h"
#include "events/eventHandler.h"
#include "events/buffers/eventBufferMap.h"

#include <memory>

using namespace ::testing;
using namespace one::client::events;

template <class EventType> class LowerLayer {
public:
    using EventT = EventType;
    using EventPtr = std::unique_ptr<EventT>;
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    void process(EventPtr event) { process(*event); }

    MOCK_METHOD1_T(process, void(const EventT &));
};

template <class EventT> class EventHandlerTest : public ::testing::Test {
public:
    EventHandlerTest()
        : buffer{std::make_unique<NiceMock<MockEventBuffer<EventT>>>()}
    {
        handler.setOnTriggerCallback([] {});
    }

protected:
    std::unique_ptr<NiceMock<MockEventBuffer<EventT>>> buffer;
    EventHandler<LowerLayer<EventT>> handler;
};

TYPED_TEST_CASE(EventHandlerTest, TestEventTypes);

TYPED_TEST(EventHandlerTest, processShouldPushEventToEventBuffer)
{
    EXPECT_CALL(*this->buffer, push(_)).Times(1);
    this->handler.setEventBuffer(std::move(this->buffer));
    this->handler.process(std::make_unique<TypeParam>("fileUuid"));
}

TYPED_TEST(
    EventHandlerTest, triggerShouldClearEventBufferAndCallOnTriggerCallback)
{
    EXPECT_CALL(*this->buffer, clear()).Times(1);
    this->handler.setEventBuffer(std::move(this->buffer));

    bool called = false;
    this->handler.setOnTriggerCallback([&] { called = true; });

    this->handler.trigger();

    EXPECT_TRUE(called);
}

TYPED_TEST(EventHandlerTest, triggerWithEventShouldCallProcessAndTrigger)
{
    EXPECT_CALL(*this->buffer, push(_)).Times(1);
    EXPECT_CALL(*this->buffer, clear()).Times(1);
    this->handler.setEventBuffer(std::move(this->buffer));

    bool called = false;
    this->handler.setOnTriggerCallback([&] { called = true; });

    this->handler.trigger(std::make_unique<TypeParam>("fileUuid"));

    EXPECT_TRUE(called);
}

TYPED_TEST(EventHandlerTest, setEventBufferShouldSetOnClearHandler)
{
    EXPECT_CALL(*this->buffer, setOnClearHandler(_)).Times(1);
    this->handler.setEventBuffer(std::move(this->buffer));
}

TYPED_TEST(EventHandlerTest, setEventHandlerShouldOverwriteDefaultHandler)
{
    bool called = false;
    this->handler.setEventBuffer(std::make_unique<EventBufferMap<TypeParam>>());
    this->handler.setEventHandler([&](const auto &) { called = true; });
    this->handler.trigger(std::make_unique<TypeParam>("fileUuid"));
    this->handler.setEventHandler([&](const auto &) {});
    EXPECT_TRUE(called);
}

TYPED_TEST(
    EventHandlerTest, onClearHandlerShouldIgnoreEventsIfEventHandlerNotSet)
{
    EXPECT_CALL(this->handler.mock, process(_)).Times(0);
    this->handler.setEventBuffer(std::make_unique<EventBufferMap<TypeParam>>());
    this->handler.trigger(std::make_unique<TypeParam>("fileUuid"));
}

TYPED_TEST(EventHandlerTest, onClearHandlerShouldExecuteEventHandler)
{
    bool called = false;
    this->handler.setEventBuffer(std::make_unique<EventBufferMap<TypeParam>>());
    this->handler.setEventHandler([&](const auto &) { called = true; });
    this->handler.trigger(std::make_unique<TypeParam>("fileUuid"));
    this->handler.setEventHandler([&](const auto &) {});
    EXPECT_TRUE(called);
}
