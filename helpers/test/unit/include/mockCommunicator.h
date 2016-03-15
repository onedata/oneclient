/**
 * @file mockCommunicator.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_TEST_MOCK_COMMUNICATOR_H
#define HELPERS_TEST_MOCK_COMMUNICATOR_H

#include "mockConnection.h"
#include "scheduler.h"
#include "communication/communicator.h"

#include <gmock/gmock.h>

#include <string>

struct MockCommunicator {
    using Callback = one::communication::Connection::Callback;

    MockCommunicator()
    {
        using namespace ::testing;

        ON_CALL(connection, send(_, _))
            .WillByDefault(Invoke(this, &MockCommunicator::sendUnwrap));

        ON_CALL(connection, connect())
            .WillByDefault(Invoke(this, &MockCommunicator::sayReady));

        communicator.setScheduler(std::make_shared<one::Scheduler>(0));
        communicator.connect();
    }

    auto &get() { return communicator; }

    MOCK_METHOD1(send, void(std::string));

    MockConnection connection;
    one::clproto::ClientMessage lastSentMessage;
    one::communication::Communicator communicator{
        1, "host", 5555, true, createMockConnectionFactory(connection)};

private:
    void sendUnwrap(
        std::string data, one::communication::Connection::Callback callback)
    {
        lastSentMessage.ParseFromString(data);
        callback({});
        sayReady();
        send(std::move(data));
    }

    void sayReady() { connection.onReady(*connection.wrapper); }
};

ACTION_P2(ReplyWith, communicator, replyMsg)
{
    one::clproto::ClientMessage clientMsg;
    clientMsg.ParseFromString(arg0);

    auto msg = replyMsg;
    msg.set_message_id(clientMsg.message_id());
    communicator->connection.onMessage(msg.SerializeAsString());
}

ACTION_P(DecodeMessage, clientMsg) { clientMsg->ParseFromString(arg0); }

#endif // HELPERS_TEST_MOCK_COMMUNICATOR_H
