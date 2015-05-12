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

#include <functional>
#include <memory>
#include <vector>

using namespace one;
using namespace one::communication;
using namespace ::testing;
using namespace std::placeholders;

struct ConnectionMock : public Connection {
    MOCK_METHOD2(connect, void(const std::string &, const std::string &));
    MOCK_METHOD1(sendProxy, void(std::string));
    MOCK_METHOD1(close, void(boost::exception_ptr));

    virtual void send(
        std::string message, boost::promise<void> promise) override
    {
        sendProxy(message);
        promise.set_value();
    }

    ConnectionMock(boost::asio::io_service &ioService,
        boost::asio::ssl::context &context, const bool verifyServerCertificate,
        std::function<std::string()> &getHandshake,
        std::function<bool(std::string)> &onHandshakeResponse,
        std::function<void(std::string)> onMessageReceived,
        std::function<void(std::shared_ptr<Connection>)> onReady,
        std::function<void(std::shared_ptr<Connection>, boost::exception_ptr)>
            onClosed)
        : Connection{ioService, context, verifyServerCertificate, getHandshake,
              onHandshakeResponse, onMessageReceived, onReady, onClosed}
        , getHandshake{getHandshake}
        , onHandshakeResponse{onHandshakeResponse}
        , onReady{onReady}
    {
    }

    std::function<std::string()> &getHandshake;
    std::function<bool(std::string)> &onHandshakeResponse;
    std::function<void(std::shared_ptr<Connection>)> onReady;
};

struct ConnectionPoolTest : public ::testing::Test {
    std::string host = randomString();
    std::string service = randomString();

    ConnectionPool connectionPool{1, host, service, true,
        std::bind(&ConnectionPoolTest::createConnectionMock, this, _1, _2, _3,
                                      _4, _5, _6, _7, _8)};

    std::shared_ptr<ConnectionMock> connection;

    std::shared_ptr<Connection> createConnectionMock(
        boost::asio::io_service &ioService, boost::asio::ssl::context &context,
        const bool verifyServerCertificate,
        std::function<std::string()> &getHandshake,
        std::function<bool(std::string)> &onHandshakeResponse,
        std::function<void(std::string)> onMessageReceived,
        std::function<void(std::shared_ptr<Connection>)> onReady,
        std::function<void(std::shared_ptr<Connection>, boost::exception_ptr)>
            onClosed)
    {
        auto conn = std::make_shared<ConnectionMock>(ioService, context,
            verifyServerCertificate, getHandshake, onHandshakeResponse,
            std::move(onMessageReceived), std::move(onReady),
            std::move(onClosed));

        EXPECT_CALL(*conn, connect(host, service));

        connection = conn;
        return conn;
    }
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
        [&] { return data; }, [](auto) { return true; });

    ASSERT_EQ(data, connection->getHandshake());

    data = randomString();

    ASSERT_EQ(data, connection->getHandshake());
}

TEST_F(ConnectionPoolTest, setHandshakeShouldPassOnResponseRefToConnections)
{
    int called = 0;
    bool handshakeReturn = false;
    auto data = randomString();

    auto onHandshakeResponse = [&](std::string msg) {
        ++called;
        EXPECT_EQ(data, msg);
        return handshakeReturn;
    };

    connectionPool.connect();
    connectionPool.setHandshake([&] { return ""; }, onHandshakeResponse);

    ASSERT_FALSE(connection->onHandshakeResponse(data));

    handshakeReturn = true;

    ASSERT_TRUE(connection->onHandshakeResponse(data));

    data = randomString();

    connection->onHandshakeResponse(data);
    ASSERT_EQ(3, called);
}

TEST_F(ConnectionPoolTest, sendShouldCallSendOnConnection)
{
    const auto data = randomString();

    connectionPool.connect();

    EXPECT_CALL(*connection, sendProxy(data));
    connection->onReady(connection);
    auto future = connectionPool.send(data);

    auto futureStatus = future.wait_for(boost::chrono::seconds(5));
    ASSERT_EQ(boost::future_status::ready, futureStatus);
}

TEST_F(ConnectionPoolTest, sendShouldNotCallSendOnConnectionBeforeOnReady)
{
    const auto data = randomString();

    connectionPool.connect();

    EXPECT_CALL(*connection, sendProxy(data)).Times(0);
    connectionPool.send(data);
}
