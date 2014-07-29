/**
 * @file messageBuilder_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef MESSAGE_BUILDER_MOCK_H
#define MESSAGE_BUILDER_MOCK_H

#include "messageBuilder.h"

#include "fuse_messages.pb.h"

#include <gmock/gmock.h>
 
#include <memory>

class MockMessageBuilder: public veil::client::MessageBuilder
{
public:
    MockMessageBuilder(std::shared_ptr<veil::client::Context> context)
        : MessageBuilder{std::move(context)}
    {
    }

    MOCK_METHOD3(createFuseMessage, veil::protocol::fuse_messages::FuseMessage(const std::string&, const std::string&, const std::string&));
    MOCK_METHOD1(decodeAtomAnswer, std::string(veil::protocol::communication_protocol::Answer&));
};

#endif // MESSAGE_BUILDER_MOCK_H
