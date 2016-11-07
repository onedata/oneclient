/**
 * @file connection_pool_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/connection.h"
#include "communication/connectionPool.h"
#include "mockConnection.h"
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
using namespace one::testing;
using namespace ::testing;
using namespace std::placeholders;
using namespace std::literals;

struct ConnectionPoolTest : public ::testing::Test {
    std::string host = randomString();
    const unsigned short port = randomInt();

    MockConnection connection;

    ConnectionPool connectionPool{
        1, host, port, true, createMockConnectionFactory(connection)};
};

TEST_F(ConnectionPoolTest, connectShouldCreateConnections)
{
    ASSERT_FALSE(connection.created);
    connectionPool.connect();
    ASSERT_TRUE(connection.created);
}

TEST_F(ConnectionPoolTest, setHandshakeShouldPassGetHandshakeToConnections)
{
    auto data = randomString();

    connectionPool.setHandshake([&] { return data; },
        [](auto) { return std::error_code{}; }, [](auto) {});
    connectionPool.connect();

    ASSERT_EQ(data, connection.getHandshake());

    data = randomString();

    ASSERT_EQ(data, connection.getHandshake());
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

    ASSERT_TRUE(!!connection.onHandshakeResponse(data));

    handshakeReturn = std::error_code{};

    ASSERT_FALSE(!!connection.onHandshakeResponse(data));

    data = randomString();

    connection.onHandshakeResponse(data);
    ASSERT_EQ(3, called);
}

TEST_F(ConnectionPoolTest, sendShouldNotCallSendOnConnectionBeforeReady)
{
    const auto data = randomString();

    connectionPool.connect();

    EXPECT_CALL(connection, send(data, _)).Times(0);
    std::thread t{[&] { connectionPool.send(data, [](auto) {}); }};

    Mock::VerifyAndClearExpectations(&connection);
    EXPECT_CALL(connection, send(data, _)).Times(1);

    connection.onReady(*connection.wrapper);
    t.join();
}
