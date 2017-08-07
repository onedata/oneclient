/**
 * @file mockConnection.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_TEST_MOCK_CONNECTION_H
#define HELPERS_TEST_MOCK_CONNECTION_H

#include "communication/connection.h"
#include "communication/connectionPool.h"

#include <gmock/gmock.h>

#include <memory>

struct MockConnectionWrapper;

struct MockConnection {
    MOCK_METHOD2(
        send, void(std::string, one::communication::Connection::Callback));
    MOCK_METHOD0(connect, void());

    std::atomic<bool> created{false};
    MockConnectionWrapper *wrapper = nullptr;

    std::string host;
    unsigned short port = 0;
    asio::ssl::context *context = nullptr;
    std::function<void(std::string)> onMessage;
    std::function<void(one::communication::Connection &)> onReady;
    std::function<std::string()> getHandshake;
    std::function<std::error_code(std::string)> onHandshakeResponse;
    std::function<void(std::error_code)> onHandshakeDone;
};

struct MockConnectionWrapper : public one::communication::Connection {
    MockConnectionWrapper(MockConnection &mockConnection)
        : m_mockConnection{mockConnection}
    {
    }

    void send(std::string data, Callback callback) override
    {
        m_mockConnection.send(std::move(data), std::move(callback));
    }

    void connect() override { m_mockConnection.connect(); }

    MockConnection &m_mockConnection;
};

one::communication::ConnectionPool::ConnectionFactory
createMockConnectionFactory(MockConnection &mockConnection)
{
    return [&](std::string host, const unsigned short port,
               std::shared_ptr<asio::ssl::context> context,
               std::function<void(std::string)> onMessage,
               std::function<void(one::communication::Connection &)> onReady,
               std::function<std::string()> getHandshake,
               std::function<std::error_code(std::string)> onHandshakeResponse,
               std::function<void(std::error_code)> onHandshakeDone) {
        mockConnection.host = std::move(host);
        mockConnection.port = port;
        mockConnection.context = context.get();
        mockConnection.onMessage = std::move(onMessage);
        mockConnection.onReady = std::move(onReady);
        mockConnection.getHandshake = std::move(getHandshake);
        mockConnection.onHandshakeResponse = std::move(onHandshakeResponse);
        mockConnection.onHandshakeDone = std::move(onHandshakeDone);

        auto conn = std::make_unique<MockConnectionWrapper>(mockConnection);

        mockConnection.wrapper = conn.get();
        mockConnection.created = true;

        return conn;
    };
}

#endif // HELPERS_TEST_MOCK_CONNECTION_H
