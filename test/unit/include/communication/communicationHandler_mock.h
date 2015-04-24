/**
 * @file communicator_mock.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_HANDLER_MOCK_H
#define HELPERS_COMMUNICATION_HANDLER_MOCK_H


#include "communicationHandler_mock.h"

#include "communication_protocol.pb.h"
#include "communication/connection.h"
#include "connectionPool_mock.h"

#include <gmock/gmock.h>

#include <memory>

struct MockCommunicationHandler: public one::communication::CommunicationHandler
{
    using Message = one::clproto::communication_protocol::ClusterMsg;
    using Answer = one::clproto::communication_protocol::Answer;

    bool autoFulfillPromise = true;
    std::unique_ptr<std::promise<std::unique_ptr<Answer>>> promise;

    MockCommunicationHandler()
        : CommunicationHandler{std::make_unique<::testing::NiceMock<ConnectionPoolMock>>(),
                               std::make_unique<::testing::NiceMock<ConnectionPoolMock>>()}
    {
        using namespace ::testing;
        ON_CALL(*this, subscribe(_)).WillByDefault(Return([]{}));
        ON_CALL(*this, addHandshake(_, _)).WillByDefault(Return([]{}));
        ON_CALL(*this, addHandshake(_, _, _)).WillByDefault(Return([]{}));
    }

    std::future<std::unique_ptr<Answer>> communicate(Message &msg, const Pool pool,
                                                     const unsigned int) override
    {
        promise = std::make_unique<std::promise<std::unique_ptr<Answer>>>();
        communicateMock(msg, pool);
        if(autoFulfillPromise)
        {
            auto value = std::make_unique<Answer>(Answer{});
            promise->set_value(std::move(value));
        }

        return promise->get_future();
    }

    MOCK_METHOD4(reply, void(const Answer&, Message&, const Pool, const unsigned int));
    MOCK_METHOD3(send, void(Message&, const Pool, const unsigned int));
    MOCK_METHOD2(communicateMock, void(Message&, const Pool));
    MOCK_METHOD1(subscribe, std::function<void()>(SubscriptionData));
    MOCK_METHOD2(addHandshake, std::function<void()>(std::function<std::unique_ptr<MockCommunicationHandler::Message>()>,
                                                     const Pool));
    MOCK_METHOD3(addHandshake, std::function<void()>(std::function<std::unique_ptr<MockCommunicationHandler::Message>()>,
                                                     std::function<std::unique_ptr<MockCommunicationHandler::Message>()>,
                                                     const Pool));
};


#endif // HELPERS_COMMUNICATION_HANDLER_MOCK_H
