/**
 * @file key_aggregator_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "utils.h"

using namespace one::client::events;

template <class T> struct KeyAggregatorTest : public ::testing::Test {
    KeyAggregator<T> aggregator;
};

TYPED_TEST_CASE(KeyAggregatorTest, AggregableTestEventTypes);

TYPED_TEST(KeyAggregatorTest, processShouldAggregateEventsWithTheSameKey)
{
    this->aggregator.process(std::make_unique<TypeParam>("1"));
    this->aggregator.process(std::make_unique<TypeParam>("1"));
    ASSERT_EQ(1, this->aggregator.flush().size());
}

TYPED_TEST(KeyAggregatorTest, processShouldNotAggregateEventsWithDifferentKeys)
{
    this->aggregator.process(std::make_unique<TypeParam>("1"));
    this->aggregator.process(std::make_unique<TypeParam>("2"));
    ASSERT_EQ(2, this->aggregator.flush().size());
}

TYPED_TEST(KeyAggregatorTest, flushShouldBeEmpty)
{
    ASSERT_TRUE(this->aggregator.flush().empty());
}
