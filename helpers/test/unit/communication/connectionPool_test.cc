/**
 * @file connectionPool_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication/connectionPool.h"

#include "communication_protocol.pb.h"
#include "communication/connection.h"
#include "communication/exception.h"
#include "testUtils.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <algorithm>
#include <atomic>
#include <condition_variable>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include <string>
#include <thread>

using namespace ::testing;
using namespace std::placeholders;

struct ConnectionMock: public one::communication::Connection
{
    static std::atomic<unsigned int> openConnections;
    static std::mutex connectionOpenedMutex;
    static std::condition_variable connectionOpened;

    ConnectionMock(std::function<void(const std::string&)> onMessageCallback,
                   std::function<void(Connection&, std::exception_ptr)> onFailCallback,
                   std::function<void(Connection&)> onOpenCallback,
                   std::function<void(Connection&)> onErrorCallback)
        : one::communication::Connection{onMessageCallback, onFailCallback,
                                          onOpenCallback, onErrorCallback}
    {
        std::lock_guard<std::mutex> guard{connectionOpenedMutex};
        ++openConnections;
        connectionOpened.notify_all();
    }

    ~ConnectionMock()
    {
        --openConnections;
    }

    using one::communication::Connection::Connection;
    using one::communication::Connection::m_onErrorCallback;
    using one::communication::Connection::m_onFailCallback;
    using one::communication::Connection::m_onMessageCallback;
    using one::communication::Connection::m_onOpenCallback;

    MOCK_METHOD1(send, void(const std::string&));

private:
    std::thread thread;
};
decltype(ConnectionMock::openConnections) ConnectionMock::openConnections{0};
decltype(ConnectionMock::connectionOpenedMutex) ConnectionMock::connectionOpenedMutex;
decltype(ConnectionMock::connectionOpened) ConnectionMock::connectionOpened;

struct ConnectionPoolProxy: public one::communication::ConnectionPool
{
    ConnectionPoolProxy(const unsigned int connectionsNumber, std::string uri)
        : ConnectionPool{connectionsNumber, std::move(uri), {}}
    {
    }


    using one::communication::ConnectionPool::ConnectionPool;
    using one::communication::ConnectionPool::addConnections;
    using one::communication::ConnectionPool::onFail;
    using one::communication::ConnectionPool::onOpen;
    using one::communication::ConnectionPool::onError;
    using one::communication::ConnectionPool::m_onMessageCallback;

    std::vector<ConnectionMock*> createdConnections;

    MOCK_METHOD0(createConnection, std::unique_ptr<one::communication::Connection>());
    std::unique_ptr<one::communication::Connection> createConnection_impl()
    {
        auto  c = new NiceMock<ConnectionMock>{
            m_onMessageCallback,
            std::bind(&ConnectionPoolProxy::onFail, this, _1, _2),
            std::bind(&ConnectionPoolProxy::onOpen, this, _1),
            std::bind(&ConnectionPoolProxy::onError, this, _1)};

        createdConnections.emplace_back(c);
        return std::unique_ptr<one::communication::Connection>{c};
    }
};

struct ConnectionPoolTest: public ::testing::Test
{
    std::unique_ptr<ConnectionPoolProxy> connectionPool;
    std::string uri;
    unsigned int connectionNumber;

    MOCK_METHOD1(onMessage, void(const std::string&));

    ConnectionPoolTest()
    {
        uri = randomString();
        connectionNumber = randomInt();

        connectionPool = std::make_unique<NiceMock<ConnectionPoolProxy>>(connectionNumber, uri);
        ON_CALL(*connectionPool, createConnection()).WillByDefault(
                    Invoke(connectionPool.get(), &ConnectionPoolProxy::createConnection_impl));

        connectionPool->setOnMessageCallback(std::bind(&ConnectionPoolTest::onMessage, this, _1));
    }

    void initConnections()
    {
        connectionPool->addConnections();
        for(auto &c: connectionPool->createdConnections)
            c->m_onOpenCallback();
    }
};

TEST_F(ConnectionPoolTest, shouldCreateExpectedNumberOfConnectionsOnAddConnections)
{
    EXPECT_CALL(*connectionPool, createConnection()).Times(connectionNumber);
    initConnections();
}

TEST_F(ConnectionPoolTest, shouldCloseAllConnectionsOnDestruction)
{
    initConnections();
    ASSERT_EQ(connectionNumber, ConnectionMock::openConnections);
    connectionPool.reset();
    ASSERT_EQ(0u, ConnectionMock::openConnections);
}

TEST_F(ConnectionPoolTest, shouldCallOnMessageCallbackOnReceivedMessage)
{
    std::string message = randomString();

    initConnections();
    EXPECT_CALL(*this, onMessage(message)).Times(1);
    connectionPool->createdConnections.front()->m_onMessageCallback(message);
}

TEST_F(ConnectionPoolTest, shouldDoNothingOnReceiveWhenReceiveMessageIsUnset)
{
    std::string message = randomString();

    connectionPool = std::make_unique<NiceMock<ConnectionPoolProxy>>(connectionNumber, uri);
    ON_CALL(*connectionPool, createConnection()).WillByDefault(
                Invoke(connectionPool.get(), &ConnectionPoolProxy::createConnection_impl));

    initConnections();
    EXPECT_CALL(*this, onMessage(_)).Times(0);
    connectionPool->createdConnections.front()->m_onMessageCallback(message);

    EXPECT_NO_THROW(connectionPool->m_onMessageCallback(message));
}

TEST_F(ConnectionPoolTest, shouldDropConnectionsOnError)
{
    auto dropConnections = randomInt(1, connectionNumber);
    initConnections();

    for(auto i = 0; i < dropConnections; ++i)
    {
        auto c = connectionPool->createdConnections.back();
        connectionPool->createdConnections.pop_back();
        c->m_onErrorCallback();
    }

    ASSERT_EQ(connectionNumber - dropConnections, ConnectionMock::openConnections);
}

TEST_F(ConnectionPoolTest, shouldDropConnectionsOnOpenFailure)
{
    auto dropConnections = randomInt(1, connectionNumber);
    connectionPool->addConnections();

    for(auto i = 0; i < dropConnections; ++i)
    {
        auto c = connectionPool->createdConnections.back();
        connectionPool->createdConnections.pop_back();
        c->m_onFailCallback({});
    }
    for(auto &c: connectionPool->createdConnections)
        c->m_onOpenCallback();

    ASSERT_EQ(connectionNumber - dropConnections, ConnectionMock::openConnections);
}

TEST_F(ConnectionPoolTest, shouldRecreateConnectionsOnSend)
{
    auto dropConnections = randomInt(1, connectionNumber - 1);
    initConnections();

    for(auto i = 0; i < dropConnections; ++i)
    {
        auto c = connectionPool->createdConnections.back();
        connectionPool->createdConnections.pop_back();
        c->m_onFailCallback({});
    }

    connectionPool->send("");

    ASSERT_EQ(connectionNumber, ConnectionMock::openConnections);
}

TEST_F(ConnectionPoolTest, shouldRecreateAllConnectionsOnSend)
{
    initConnections();

    for(auto &c: connectionPool->createdConnections)
        c->m_onErrorCallback();

    connectionPool->createdConnections.clear();

    ASSERT_EQ(0u, ConnectionMock::openConnections);

    auto openConnections = [&]{
        std::unique_lock<std::mutex> lock{ConnectionMock::connectionOpenedMutex};
        const auto res = ConnectionMock::connectionOpened.wait_for(lock,
                            std::chrono::seconds{5},
                            [&]{ return ConnectionMock::openConnections == connectionNumber; });

        ASSERT_TRUE(res);

        for(auto &c: connectionPool->createdConnections)
            c->m_onOpenCallback();
    };

    std::thread t{openConnections};
    connectionPool->send("");
    t.join();

    ASSERT_EQ(connectionNumber, ConnectionMock::openConnections);
}

TEST_F(ConnectionPoolTest, shouldWaitOnSendUntilConnectionIsAvailable)
{
    std::atomic<bool> sent{false};

    auto openConnections = [&]{
        std::unique_lock<std::mutex> lock{ConnectionMock::connectionOpenedMutex};
        const auto res = ConnectionMock::connectionOpened.wait_for(lock,
                            std::chrono::seconds{5},
                            [&]{ return ConnectionMock::openConnections == connectionNumber; });

        ASSERT_TRUE(res);
        ASSERT_FALSE(sent);

        for(const auto &c: connectionPool->createdConnections)
            c->m_onOpenCallback();
    };

    std::thread t{openConnections};

    ASSERT_NO_THROW(connectionPool->send(""));
    sent = true;

    t.join();
}

ACTION_P(Increment, counter) { ++*counter; }

TEST_F(ConnectionPoolTest, shouldPassMessageToOneOfItsConnections)
{
    std::string message = randomString();

    std::atomic<unsigned int> sentMessages{0};
    initConnections();

    for(auto &c: connectionPool->createdConnections)
        EXPECT_CALL(*c, send(message)).WillRepeatedly(Increment(&sentMessages));

    for(unsigned int i = 1, toSend = randomInt(); i <= toSend; ++i)
    {
        connectionPool->send(message);
        ASSERT_EQ(i, sentMessages);
    }
}

TEST_F(ConnectionPoolTest, shouldDistributeMessagesEvenlyAcrossConnections)
{
    std::string message = randomString();

    initConnections();

    for(auto &c: connectionPool->createdConnections)
        EXPECT_CALL(*c, send(message)).Times(3);

    for(auto i = 0u; i < connectionNumber * 3; ++i)
        connectionPool->send(message);
}

TEST_F(ConnectionPoolTest, shouldCallHandhakeOnAllOpenConnectionsOnHandshakeAddition)
{
    std::string handshake = randomString();
    std::string handshake2 = randomString();
    initConnections();

    for(auto &c: connectionPool->createdConnections)
    {
        EXPECT_CALL(*c, send(handshake));
        EXPECT_CALL(*c, send(handshake2));
    }

    connectionPool->addHandshake([&]{ return handshake; }, []{ return "goodbye"; });
    connectionPool->addHandshake([&]{ return handshake2; });

    for(auto &c: connectionPool->createdConnections)
        Mock::VerifyAndClearExpectations(c);
}

TEST_F(ConnectionPoolTest, shouldCallHandhakeOnConnectionOpen)
{
    std::string handshake = randomString();
    connectionPool->addConnections();

    for(auto &c: connectionPool->createdConnections)
        EXPECT_CALL(*c, send(_)).Times(0);

    connectionPool->addHandshake([&]{ return handshake; });

    for(auto &c: connectionPool->createdConnections)
    {
        Mock::VerifyAndClearExpectations(c);
        EXPECT_CALL(*c, send(handshake));
        c->m_onOpenCallback();
    }
}

TEST_F(ConnectionPoolTest, shouldReturnAFunctionToDeregisterHandshake)
{
    for(auto &c: connectionPool->createdConnections)
        EXPECT_CALL(*c, send(_)).Times(0);

    auto remove = connectionPool->addHandshake([&]{ return randomString(); });
    auto remove2 = connectionPool->addHandshake([&]{ return randomString(); },
                                                [&]{ return randomString(); });

    remove();
    remove2();

    for(auto &c: connectionPool->createdConnections)
        c->m_onOpenCallback();
}

TEST_F(ConnectionPoolTest, shouldCallGoodbyeOnDeregisteringHandshake)
{
    const auto goodbye = randomString();

    auto remove = connectionPool->addHandshake([&]{ return randomString(); },
                                               [&]{ return goodbye; });

    for(auto &c: connectionPool->createdConnections)
    {
        c->m_onOpenCallback();
        EXPECT_CALL(*c, send(goodbye));
    }

    remove();

    for(auto &c: connectionPool->createdConnections)
        Mock::VerifyAndClearExpectations(c);
}

TEST_F(ConnectionPoolTest, shouldCallGoodbyeOnConnectionClose)
{
    std::string handshake = randomString();
    std::string goodbye = randomString();
    initConnections();

    connectionPool->addHandshake([&]{ return handshake; }, [&]{ return goodbye; });

    for(auto &c: connectionPool->createdConnections)
        EXPECT_CALL(*c, send(goodbye));

    connectionPool.reset();
}

TEST_F(ConnectionPoolTest, shouldCallHandshakesInFIFOOrder)
{
    std::string handshake = randomString();
    std::string handshake2 = randomString();
    connectionPool->addConnections();

    connectionPool->addHandshake([&]{ return handshake; });
    connectionPool->addHandshake([&]{ return handshake2; });

    for(auto &c: connectionPool->createdConnections)
    {
        InSequence s;
        EXPECT_CALL(*c, send(handshake));
        EXPECT_CALL(*c, send(handshake2));
    }

    for(auto &c: connectionPool->createdConnections)
        c->m_onOpenCallback();
}

TEST_F(ConnectionPoolTest, shouldCallGoodbyesInLIFOOrder)
{
    std::string goodbye = randomString();
    std::string goodbye2 = randomString();
    initConnections();

    connectionPool->addHandshake([]{ return "handshake"; }, [&]{ return goodbye; });
    connectionPool->addHandshake([]{ return "handshake"; }, [&]{ return goodbye2; });

    for(auto &c: connectionPool->createdConnections)
    {
        InSequence s;
        EXPECT_CALL(*c, send(goodbye2));
        EXPECT_CALL(*c, send(goodbye));
    }

    connectionPool.reset();
}
