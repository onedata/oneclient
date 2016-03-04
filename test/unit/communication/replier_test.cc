/**
 * @file replier_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/declarations.h"
#include "communication/layers/replier.h"
#include "testUtils.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

using namespace one;
using namespace one::communication;
using namespace one::testing;
using namespace ::testing;

struct LowerLayer {
    using Callback = std::function<void(const std::error_code &)>;

    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD2(sendProxy, void(clproto::ClientMessage, int));

    void send(ClientMessagePtr cmp, Callback /*callback*/, int i)
    {
        sendProxy(*cmp, i);
    }
};

struct ReplierTest : public ::testing::Test {
    layers::Replier<LowerLayer> replier;
};

TEST_F(ReplierTest, replyShouldSetMessageId)
{
    const auto messageId = randomString();

    clproto::ClientMessage sentMsg;
    EXPECT_CALL(replier.mock, sendProxy(_, _)).WillOnce(SaveArg<0>(&sentMsg));

    clproto::ServerMessage replyTo;
    replyTo.set_message_id(messageId);
    auto msg = std::make_unique<clproto::ClientMessage>();
    replier.reply(replyTo, std::move(msg), {}, 1);

    ASSERT_EQ(messageId, sentMsg.message_id());
}

TEST_F(ReplierTest, replyShouldPassUninterestingValuesDown)
{
    const auto retries = randomInt();
    EXPECT_CALL(replier.mock, sendProxy(_, retries)).Times(1);

    clproto::ServerMessage replyTo;
    replyTo.set_message_id(randomString());
    auto msg = std::make_unique<clproto::ClientMessage>();
    replier.reply(replyTo, std::move(msg), {}, retries);
}
