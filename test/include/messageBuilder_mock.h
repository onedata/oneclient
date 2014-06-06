/**
 * @file messageBuilder_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef MESSAGE_BUILDER_MOCK_H
#define MESSAGE_BUILDER_MOCK_H

#include "messageBuilder.h"
#include "testCommon.h"
#include "gmock/gmock.h"

#include "context.h"
 
#include <memory>

class MockMessageBuilder
    : public MessageBuilder {
public:
    MockMessageBuilder(std::shared_ptr<Context> context)
    	: MessageBuilder{std::move(context)} {};
    ~MockMessageBuilder() {};

    MOCK_METHOD4(packFuseMessage, ClusterMsg(const string&, const string&, const string&, const string&));
    MOCK_METHOD1(decodeAtomAnswer, string(Answer&));
};

#endif // MESSAGE_BUILDER_MOCK_H
