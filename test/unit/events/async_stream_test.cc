/**
 * @file async_stream_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "mocks/stream_mock.h"
#include "utils.h"

using namespace one::client::events;

struct AsyncStreamTest : public ::testing::Test {
    MockAsyncStream *mockStream = new MockAsyncStream();
    std::size_t threadId = mockStream->hasher(std::this_thread::get_id());
    AsyncStream stream{std::unique_ptr<MockAsyncStream>(mockStream)};
};

TEST_F(AsyncStreamTest, processShouldForwardCall)
{
    this->stream.process(std::make_unique<TestFileRead>("1"));
    ASSERT_TRUE(this->mockStream->processCalled.get_future().get());
    ASSERT_NE(this->threadId, this->mockStream->threadId.get_future().get());
}

TEST_F(AsyncStreamTest, flushShouldForwardCall)
{
    this->stream.flush();
    ASSERT_TRUE(this->mockStream->flushCalled.get_future().get());
    ASSERT_NE(this->threadId, this->mockStream->threadId.get_future().get());
}
