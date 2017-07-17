/**
 * @file shared_stream_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "mocks/aggregator_mock.h"
#include "mocks/emitter_mock.h"
#include "mocks/handler_mock.h"
#include "utils.h"

using namespace one::client::events;

template <class T> struct TypedStreamTest : public ::testing::Test {
    MockAggregator<T> *mockAggregator = new MockAggregator<T>();
    MockEmitter<T> *mockEmitter = new MockEmitter<T>();
    MockHandler<T> *mockHandler = new MockHandler<T>();
    TypedStream<T> stream{std::unique_ptr<MockAggregator<T>>(mockAggregator),
        std::unique_ptr<MockEmitter<T>>(mockEmitter),
        std::unique_ptr<MockHandler<T>>(mockHandler)};
};

TYPED_TEST_CASE(TypedStreamTest, TestEventTypes);

TYPED_TEST(TypedStreamTest, processShouldAggregateEvent)
{
    this->stream.process(std::make_unique<TypeParam>("1"));
    ASSERT_TRUE(this->mockEmitter->processCalled);
    ASSERT_TRUE(this->mockEmitter->readyCalled);
    ASSERT_FALSE(this->mockEmitter->resetCalled);
    ASSERT_TRUE(this->mockAggregator->processCalled);
    ASSERT_FALSE(this->mockHandler->processCalled);
}

TYPED_TEST(TypedStreamTest, processShouldFlushStream)
{
    this->mockEmitter->readyReturn = true;
    this->stream.process(std::make_unique<TypeParam>("1"));
    ASSERT_TRUE(this->mockEmitter->processCalled);
    ASSERT_TRUE(this->mockEmitter->readyCalled);
    ASSERT_TRUE(this->mockEmitter->resetCalled);
    ASSERT_TRUE(this->mockAggregator->processCalled);
    ASSERT_TRUE(this->mockAggregator->flushCalled);
    ASSERT_TRUE(this->mockHandler->processCalled);
}

TYPED_TEST(TypedStreamTest, flushShouldFlushStream)
{
    this->stream.flush();
    ASSERT_TRUE(this->mockEmitter->resetCalled);
    ASSERT_TRUE(this->mockAggregator->flushCalled);
    ASSERT_TRUE(this->mockHandler->processCalled);
}
