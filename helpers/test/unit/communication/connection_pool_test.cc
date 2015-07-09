/**
 * @file connection_pool_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/connection.h"
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

struct ConnectionMock : public Connection {
    MOCK_METHOD3(connect,
        void(std::string, std::string, std::function<void(Connection::Ptr)>));
    MOCK_METHOD1(sendProxy, void(std::string));
    MOCK_METHOD1(close, void(const std::error_code &));

    virtual void send(std::string message,
        std::function<void(const std::error_code &, Connection::Ptr)> callback)
        override
    {
        sendProxy(message);
        callback(std::error_code{}, Connection::shared_from_this());
    }

    ConnectionMock(asio::io_service &ioService, asio::ssl::context &context,
        const bool verifyServerCertificate,
        std::function<std::string()> &getHandshake,
        std::function<std::error_code(std::string)> &onHandshakeResponse,
        std::function<void(std::string)> onMessageReceived,
        std::function<void(Connection::Ptr, const std::error_code &)> onClosed)
        : Connection{ioService, context, verifyServerCertificate, getHandshake,
              onHandshakeResponse, onMessageReceived, onClosed}
        , getHandshake{getHandshake}
        , onHandshakeResponse{onHandshakeResponse}
    {
    }

    std::function<std::string()> &getHandshake;
    std::function<std::error_code(std::string)> &onHandshakeResponse;
};

struct ConnectionPoolTest : public ::testing::Test {
    std::string host = randomString();
    std::string service = randomString();

    ConnectionPool connectionPool{1, host, service, true,
        std::bind(&ConnectionPoolTest::createConnectionMock, this, _1, _2, _3,
                                      _4, _5, _6, _7)};

    std::shared_ptr<ConnectionMock> connection;

    std::shared_ptr<Connection> createConnectionMock(
        asio::io_service &ioService, asio::ssl::context &context,
        const bool verifyServerCertificate,
        std::function<std::string()> &getHandshake,
        std::function<std::error_code(std::string)> &onHandshakeResponse,
        std::function<void(std::string)> onMessageReceived,
        std::function<void(Connection::Ptr, const std::error_code &)> onClosed)
    {
        auto conn = std::make_shared<ConnectionMock>(ioService, context,
            verifyServerCertificate, getHandshake, onHandshakeResponse,
            std::move(onMessageReceived), std::move(onClosed));

        EXPECT_CALL(*conn, connect(host, service, _))
            .WillOnce(SaveArg<2>(&onConnect));

        connection = conn;
        return conn;
    }

    std::function<void(Connection::Ptr)> onConnect;
};

TEST_F(ConnectionPoolTest, connectShouldCreateConnections)
{
    ASSERT_FALSE(!!connection);
    connectionPool.connect();
    ASSERT_TRUE(!!connection);
}

TEST_F(ConnectionPoolTest, setHandshakeShouldPassGetHandshakeRefToConnections)
{
    auto data = randomString();

    connectionPool.connect();
    connectionPool.setHandshake(
        [&] { return data; }, [](auto) { return std::error_code{}; });

    ASSERT_EQ(data, connection->getHandshake());

    data = randomString();

    ASSERT_EQ(data, connection->getHandshake());
}

TEST_F(ConnectionPoolTest, setHandshakeShouldPassOnResponseRefToConnections)
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

    connectionPool.connect();
    connectionPool.setHandshake([&] { return ""; }, onHandshakeResponse);

    ASSERT_TRUE(!!connection->onHandshakeResponse(data));

    handshakeReturn = std::error_code{};

    ASSERT_FALSE(!!connection->onHandshakeResponse(data));

    data = randomString();

    connection->onHandshakeResponse(data);
    ASSERT_EQ(3, called);
}

TEST_F(ConnectionPoolTest, sendShouldCallSendOnConnection)
{
    const auto data = randomString();

    connectionPool.connect();

    EXPECT_CALL(*connection, sendProxy(data));
    onConnect(connection);

    std::atomic<bool> sent{false};
    connectionPool.send(data, [&](auto ec) {
        if (!ec)
            sent = true;
    });

    auto now = std::chrono::steady_clock::now();
    while (!sent && now + 5s > std::chrono::steady_clock::now())
        std::this_thread::sleep_for(1ms);

    ASSERT_TRUE(sent);
}

TEST_F(ConnectionPoolTest, sendShouldNotCallSendOnConnectionBeforeReady)
{
    const auto data = randomString();

    connectionPool.connect();

    EXPECT_CALL(*connection, sendProxy(data)).Times(0);
    connectionPool.send(data, [](auto) {});
}
