/**
 * @file counter_emitter_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "mocks/emitter_mock.h"
#include "utils.h"

using namespace one::client::events;

template <class T> struct CounterEmitterTest : public ::testing::Test {
    MockEmitter<T> *mockEmitter = new MockEmitter<T>();
    CounterEmitter<T> emitter{2, std::unique_ptr<MockEmitter<T>>(mockEmitter)};
};

TYPED_TEST_CASE(CounterEmitterTest, TestEventTypes);

TYPED_TEST(CounterEmitterTest, readyShouldReturnFalse)
{
    ASSERT_FALSE(this->emitter.ready());
}

TYPED_TEST(CounterEmitterTest, readyShouldForwardCall)
{
    this->emitter.ready();
    ASSERT_TRUE(this->mockEmitter->readyCalled);
}

TYPED_TEST(CounterEmitterTest, readyShouldNotForwardCall)
{
    this->emitter.process(std::make_unique<TypeParam>("1"));
    this->emitter.process(std::make_unique<TypeParam>("2"));
    this->emitter.ready();
    ASSERT_FALSE(this->mockEmitter->readyCalled);
}

TYPED_TEST(CounterEmitterTest, processShouldNotChangeReadyStatus)
{
    this->emitter.process(std::make_unique<TypeParam>("1"));
    ASSERT_FALSE(this->emitter.ready());
}

TYPED_TEST(CounterEmitterTest, processShouldChangeReadyStatus)
{
    this->emitter.process(std::make_unique<TypeParam>("1"));
    this->emitter.process(std::make_unique<TypeParam>("2"));
    ASSERT_TRUE(this->emitter.ready());
}

TYPED_TEST(CounterEmitterTest, processShouldForwardCall)
{
    this->emitter.process(std::make_unique<TypeParam>("1"));
    ASSERT_TRUE(this->mockEmitter->processCalled);
}

TYPED_TEST(CounterEmitterTest, resetShouldWipeEmitter)
{
    this->emitter.process(std::make_unique<TypeParam>("1"));
    this->emitter.reset();
    this->emitter.process(std::make_unique<TypeParam>("2"));
    ASSERT_FALSE(this->emitter.ready());
}

TYPED_TEST(CounterEmitterTest, resetShouldChangeReadyStatus)
{
    this->emitter.process(std::make_unique<TypeParam>("1"));
    this->emitter.process(std::make_unique<TypeParam>("2"));
    ASSERT_TRUE(this->emitter.ready());
    this->emitter.reset();
    ASSERT_FALSE(this->emitter.ready());
}

TYPED_TEST(CounterEmitterTest, resetShouldForwardCall)
{
    this->emitter.reset();
    ASSERT_TRUE(this->mockEmitter->resetCalled);
}
