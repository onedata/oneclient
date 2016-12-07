/**
 * @file shared_stream_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "mocks/stream_mock.h"
#include "utils.h"

using namespace one::client::events;

struct SharedStreamTest : public ::testing::Test {
    MockStream *mockStream = new MockStream();
    SharedStream stream{std::unique_ptr<MockStream>(mockStream)};
};

TEST_F(SharedStreamTest, processShouldForwardCall)
{
    this->stream.process(std::make_unique<TestFileRead>("1"));
    ASSERT_TRUE(this->mockStream->processCalled);
}

TEST_F(SharedStreamTest, flushShouldForwardCall)
{
    this->stream.flush();
    ASSERT_TRUE(this->mockStream->flushCalled);
}

TEST_F(SharedStreamTest, releaseLastShareShouldReturnTrue)
{
    ASSERT_TRUE(this->stream.release());
}

TEST_F(SharedStreamTest, shareShouldPreventRelease)
{
    this->stream.share();
    ASSERT_FALSE(this->stream.release());
}
