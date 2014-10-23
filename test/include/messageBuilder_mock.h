/**
 * @file messageBuilder_mock.h
 * @author Rafal Slota
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef MESSAGE_BUILDER_MOCK_H
#define MESSAGE_BUILDER_MOCK_H


#include "messageBuilder.h"

#include <gmock/gmock.h>

class MockMessageBuilder: public one::client::MessageBuilder
{
public:
    MockMessageBuilder(std::weak_ptr<one::client::Context> ctx)
        : MessageBuilder(ctx)
    {
    }

    MOCK_CONST_METHOD1(createFuseMessage, one::clproto::fuse_messages::FuseMessage(
                     const google::protobuf::Message&));

    MOCK_CONST_METHOD1(decodeAtomAnswer, std::string(
                     const one::clproto::communication_protocol::Answer&));
};


#endif // MESSAGE_BUILDER_MOCK_H
