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

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <atomic>
#include <string>

using namespace one;
using namespace one::communication;
using namespace one::testing;
using namespace ::testing;

struct LowerLayer {
    using Callback = std::function<void(const std::error_code &)>;

    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD2(sendProxy, bool(std::string, int));

    void send(std::string msg, Callback callback, int i)
    {
        if (!sendProxy(std::move(msg), i))
            callback(std::make_error_code(std::errc::owner_dead));
        else
            callback(std::error_code{});
    }
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

    retrier.send(randomString(), [](auto &ec) { ASSERT_FALSE(!!ec); }, 2);
}

TEST_F(RetrierTest, sendShouldFinishOnFirstSuccess)
{
    EXPECT_CALL(retrier.mock, sendProxy(_, _))
        .WillOnce(Return(false))
        .WillOnce(Return(true));

    retrier.send(randomString(), [](auto &ec) { ASSERT_FALSE(!!ec); }, 30);
}

TEST_F(RetrierTest, sendShouldPassExceptionAfterUnsuccessfulRetries)
{
    EXPECT_CALL(retrier.mock, sendProxy(_, _))
        .WillOnce(Return(false))
        .WillOnce(Return(false));

    retrier.send(randomString(), [](auto &ec) { ASSERT_TRUE(!!ec); }, 1);
}

TEST_F(RetrierTest, sendShouldPassUninterestingArgumentsDown)
{
    const auto msg = randomString();

    EXPECT_CALL(retrier.mock, sendProxy(msg, _))
        .WillOnce(Return(false))
        .WillOnce(Return(false))
        .WillOnce(Return(true));

    retrier.send(msg, [](auto &ec) { ASSERT_FALSE(!!ec); }, 3);
}
