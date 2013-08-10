/**
 * @file messageBuilder_mock.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef MESSAGE_BUILDER_MOCK_H
#define MESSAGE_BUILDER_MOCK_H

#include "messageBuilder.hh"
#include "gmock/gmock.h"

class MockMessageBuilder
    : public MessageBuilder {
public:
    MockMessageBuilder() {};
    ~MockMessageBuilder() {};

    MOCK_METHOD4(packFuseMessage, ClusterMsg* (string, string, string, string));
    MOCK_METHOD1(decodeAtomAnswer, string(Answer&));
};

#endif // MESSAGE_BUILDER_MOCK_H