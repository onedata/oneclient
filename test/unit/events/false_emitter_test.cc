/**
 * @file false_emitter_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "utils.h"

using namespace one::client::events;

template <class T> struct FalseEmitterTest : public ::testing::Test {
    FalseEmitter<T> emitter;
};

TYPED_TEST_CASE(FalseEmitterTest, TestEventTypes);

TYPED_TEST(FalseEmitterTest, readyShouldReturnFalse)
{
    ASSERT_FALSE(this->emitter.ready());
}

TYPED_TEST(FalseEmitterTest, processShouldNotChangeReadyStatus)
{
    this->emitter.process(std::make_unique<TypeParam>("1"));
    ASSERT_FALSE(this->emitter.ready());
}

TYPED_TEST(FalseEmitterTest, resetShouldNotChangeReadyStatus)
{
    ASSERT_FALSE(this->emitter.ready());
    this->emitter.reset();
    ASSERT_FALSE(this->emitter.ready());
}
