/**
 * @file asyncResponder_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/layers/asyncResponder.h"
#include "testUtils.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <future>
#include <thread>

using namespace one;
using namespace one::communication;
using namespace one::testing;
using namespace ::testing;

struct LowerLayer {
    using Callback = std::function<void(const std::error_code &)>;

    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD0(connect, void());
    MOCK_METHOD1(
        setOnMessageCallback, void(std::function<void(ServerMessagePtr)>));
};

struct AsyncResponderTest : public ::testing::Test {
    layers::AsyncResponder<LowerLayer> responder;
};

TEST_F(AsyncResponderTest, messageShouldBeHandledInAThread)
{
    std::function<void(ServerMessagePtr)> onMessageCallback;
    EXPECT_CALL(responder.mock, connect());
    EXPECT_CALL(responder.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&onMessageCallback));

    responder.connect();

    auto threadIdPromise = std::make_shared<std::promise<std::thread::id>>();
    responder.setOnMessageCallback(
        [&](auto) { threadIdPromise->set_value(std::this_thread::get_id()); });

    onMessageCallback(nullptr);
    ASSERT_NE(std::this_thread::get_id(), threadIdPromise->get_future().get());
}
