/**
 * @file connection_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication/connection.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <functional>
#include <memory>
#include <string>

using namespace ::testing;
using namespace std::placeholders;

struct Connection: public one::communication::Connection
{
    using one::communication::Connection::Connection;
    using one::communication::Connection::m_onErrorCallback;
    using one::communication::Connection::m_onFailCallback;
    using one::communication::Connection::m_onMessageCallback;
    using one::communication::Connection::m_onOpenCallback;

    MOCK_METHOD1(send, void(const std::string&));
};

struct ConnectionTest: public ::testing::Test
{
    std::unique_ptr<Connection> connection;

    MOCK_METHOD1(onMessage, void(const std::string&));
    MOCK_METHOD1(onFail, void(one::communication::Connection&));
    MOCK_METHOD1(onOpen, void(one::communication::Connection&));
    MOCK_METHOD1(onError, void(one::communication::Connection&));

    ConnectionTest()
    {
        connection = std::make_unique<Connection>(
                    std::bind(&ConnectionTest::onMessage, this, _1),
                    std::bind(&ConnectionTest::onFail, this, _1),
                    std::bind(&ConnectionTest::onOpen, this, _1),
                    std::bind(&ConnectionTest::onError, this, _1));
    }
};

bool identityEqual(const one::communication::Connection &lhs,
                   const one::communication::Connection &rhs)
{
    return &lhs == &rhs;
}

TEST_F(ConnectionTest, shouldBindItselfToOnFailCallback)
{
    const auto isTheConnection = std::bind(identityEqual, _1, std::cref(*connection));

    EXPECT_CALL(*this, onFail(Truly(isTheConnection)));
    connection->m_onFailCallback({});
}

TEST_F(ConnectionTest, shouldBindItselfToOnOpenCallback)
{
    const auto isTheConnection = std::bind(identityEqual, _1, std::cref(*connection));

    EXPECT_CALL(*this, onOpen(Truly(isTheConnection)));
    connection->m_onOpenCallback();
}

TEST_F(ConnectionTest, shouldBindItselfToOnErrorCallback)
{
    const auto isTheConnection = std::bind(identityEqual, _1, std::cref(*connection));

    EXPECT_CALL(*this, onError(Truly(isTheConnection)));
    connection->m_onErrorCallback();
}
