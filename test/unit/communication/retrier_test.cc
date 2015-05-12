/**
 * @file retrier_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/exception.h"
#include "communication/layers/retrier.h"
#include "testUtils.h"

#include <boost/thread/future.hpp>
#include <boost/thread/executors/basic_thread_pool.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>

using namespace one;
using namespace one::communication;
using namespace ::testing;

struct LowerLayer {
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD2(sendProxy, bool(std::string, int));

    boost::future<void> send(std::string msg, int i)
    {
        if (!sendProxy(std::move(msg), i))
            return boost::make_exceptional(ConnectionError{exceptionText});

        return boost::make_ready_future();
    }

    boost::basic_thread_pool m_ioServiceExecutor{1};
    std::string exceptionText = "error";
};

struct RetrierTest : public ::testing::Test {
    layers::Retrier<LowerLayer> retrier;
};

TEST_F(RetrierTest, sendShouldRetryCommunicationOnError)
{
    EXPECT_CALL(retrier.mock, sendProxy(_, _))
        .WillOnce(Return(false))
        .WillOnce(Return(false))
        .WillOnce(Return(true));

    retrier.send(randomString(), 2).get();
}

TEST_F(RetrierTest, sendShouldFinishOnFirstSuccess)
{
    EXPECT_CALL(retrier.mock, sendProxy(_, _))
        .WillOnce(Return(false))
        .WillOnce(Return(true));

    retrier.send(randomString(), 30).get();
}

TEST_F(RetrierTest, sendShouldPassExceptionAfterUnsuccessfulRetries)
{
    EXPECT_CALL(retrier.mock, sendProxy(_, _))
        .WillOnce(Return(false))
        .WillOnce(Return(false));

    retrier.mock.exceptionText = randomString();

    try {
        retrier.send(randomString(), 1).get();
        FAIL();
    }
    catch (const ConnectionError &e) {
        ASSERT_EQ(retrier.mock.exceptionText, e.what());
    }
}

TEST_F(RetrierTest, sendShouldPassUninterestingArgumentsDown)
{
    const auto msg = randomString();

    EXPECT_CALL(retrier.mock, sendProxy(msg, _))
        .WillOnce(Return(false))
        .WillOnce(Return(false))
        .WillOnce(Return(true));

    retrier.send(msg, 3).get();
}
