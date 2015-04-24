/**
 * @file connectionPool_mock.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CONNECTION_POOL_MOCK_H
#define HELPERS_COMMUNICATION_CONNECTION_POOL_MOCK_H


#include "communication/communicator.h"

#include <gmock/gmock.h>

#include <functional>
#include <string>

struct ConnectionPoolMock: public one::communication::ConnectionPool
{
    ConnectionPoolMock()
        : one::communication::ConnectionPool{1, "uri", {}}
    {
        using namespace ::testing;

        ON_CALL(*this, setOnMessageCallback(_)).
                WillByDefault(SaveArg<0>(&onMessageCallback));

        ON_CALL(*this, addHandshake(_)).WillByDefault(Return([]{}));
        ON_CALL(*this, addHandshake(_, _)).WillByDefault(Return([]{}));
    }

    std::function<void(const std::string&)> onMessageCallback;

    MOCK_METHOD1(send, void(const std::string&));
    MOCK_METHOD1(setOnMessageCallback, void(std::function<void(const std::string&)>));
    MOCK_METHOD1(addHandshake, std::function<void()>(std::function<std::string()> handshake));
    MOCK_METHOD2(addHandshake, std::function<void()>(std::function<std::string()> handshake,
                                                     std::function<std::string()> goodbye));

    MOCK_METHOD0(createConnection, std::unique_ptr<one::communication::Connection>());
};


#endif // HELPERS_COMMUNICATION_CONNECTION_POOL_MOCK_H
