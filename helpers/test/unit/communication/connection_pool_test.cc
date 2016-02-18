/**
 * @file connection_pool_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/persistentConnection.h"
#include "communication/connectionPool.h"
#include "testUtils.h"

#include <boost/thread/future.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <atomic>
#include <chrono>
#include <functional>
#include <memory>
#include <vector>

using namespace one;
using namespace one::communication;
using namespace ::testing;
using namespace std::placeholders;
using namespace std::literals;

struct ConnectionMock : public PersistentConnection {
    MOCK_METHOD0(connect, void());
    MOCK_METHOD2(send, void(std::string, PersistentConnection::Callback));

    ConnectionMock(std::string host, const unsigned short port,
        asio::ssl::context &context, std::function<void(std::string)> onMessage,
        std::function<void(PersistentConnection &)> onReady_,
        std::function<std::string()> getHandshake_,
        std::function<std::error_code(std::string)> onHandshakeResponse_,
        std::function<void(std::error_code)> onHandshakeDone)
        : PersistentConnection{std::move(host), port, context,
              std::move(onMessage), onReady_, getHandshake_,
              onHandshakeResponse_, std::move(onHandshakeDone)}
        , onReady{onReady_}
        , getHandshake{getHandshake_}
        , onHandshakeResponse{onHandshakeResponse_}
    {
    }

    std::function<void(PersistentConnection &)> onReady;
    std::function<std::string()> getHandshake;
    std::function<std::error_code(std::string)> onHandshakeResponse;
};

struct ConnectionPoolTest : public ::testing::Test {
    std::string host = randomString();
    const unsigned short port = randomInt();

    ConnectionPool connectionPool{1, host, port, true,
        std::bind(&ConnectionPoolTest::createConnectionMock, this, _1, _2, _3,
                                      _4, _5, _6, _7, _8)};

    ConnectionMock *connection = nullptr;

    std::unique_ptr<PersistentConnection> createConnectionMock(
        std::string host_, const unsigned short port_,
        asio::ssl::context &context, std::function<void(std::string)> onMessage,
        std::function<void(PersistentConnection &)> onReady,
        std::function<std::string()> getHandshake,
        std::function<std::error_code(std::string)> onHandshakeResponse,
        std::function<void(std::error_code)> onHandshakeDone)
    {
        std::unique_ptr<PersistentConnection> conn =
            std::make_unique<ConnectionMock>(std::move(host_), port_, context,
                std::move(onMessage), std::move(onReady),
                std::move(getHandshake), std::move(onHandshakeResponse),
                std::move(onHandshakeDone));

        connection = static_cast<ConnectionMock *>(conn.get());
        return conn;
    }
};

TEST_F(ConnectionPoolTest, connectShouldCreateConnections)
{
    ASSERT_FALSE(!!connection);
    connectionPool.connect();
    ASSERT_TRUE(!!connection);
}

TEST_F(ConnectionPoolTest, setHandshakeShouldPassGetHandshakeToConnections)
{
    auto data = randomString();

    connectionPool.setHandshake([&] { return data; },
        [](auto) { return std::error_code{}; }, [](auto) {});
    connectionPool.connect();

    ASSERT_EQ(data, connection->getHandshake());

    data = randomString();

    ASSERT_EQ(data, connection->getHandshake());
}

TEST_F(ConnectionPoolTest, setHandshakeShouldPassOnResponseToConnections)
{
    int called = 0;
    std::error_code handshakeReturn =
        std::make_error_code(std::errc::protocol_error);

    auto data = randomString();

    auto onHandshakeResponse = [&](std::string msg) {
        ++called;
        EXPECT_EQ(data, msg);
        return handshakeReturn;
    };

    connectionPool.setHandshake(
        [&] { return ""; }, onHandshakeResponse, [](auto) {});
    connectionPool.connect();

    ASSERT_TRUE(!!connection->onHandshakeResponse(data));

    handshakeReturn = std::error_code{};

    ASSERT_FALSE(!!connection->onHandshakeResponse(data));

    data = randomString();

    connection->onHandshakeResponse(data);
    ASSERT_EQ(3, called);
}

TEST_F(ConnectionPoolTest, sendShouldNotCallSendOnConnectionBeforeReady)
{
    const auto data = randomString();

    connectionPool.connect();

    EXPECT_CALL(*connection, send(data, _)).Times(0);
    std::thread t{[&] { connectionPool.send(data, [](auto) {}); }};

    Mock::VerifyAndClearExpectations(connection);
    EXPECT_CALL(*connection, send(data, _)).Times(1);

    connection->onReady(*connection);
    t.join();
}
