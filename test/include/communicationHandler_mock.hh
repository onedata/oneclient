/**
 * @file communicationHandler_mock.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef COMMUNICATION_HANDLER_MOCK_H
#define COMMUNICATION_HANDLER_MOCK_H

#include "communicationHandler.hh"
#include "gmock/gmock.h"

class MockCommunicationHandler
    : public CommunicationHandler {
public:
    MOCK_METHOD2(communicate, Answer(ClusterMsg&, uint8_t));
};

#endif // COMMUNICATION_HANDLER_MOCK_H