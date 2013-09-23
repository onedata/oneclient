/**
 * @file fslogicProxy_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef FSLOGIC_PROXY_PROXY_H
#define FSLOGIC_PROXY_PROXY_H

#include "fslogicProxy.h"
#include "communicationHandler_mock.h"
#include "messageBuilder_mock.h"
#include "testCommon.h"
#include "gmock/gmock.h"

using namespace boost;

class ProxyFslogicProxy
    : public FslogicProxy {
public:
    bool useMockConnectionSelector;
    shared_ptr<MockCommunicationHandler> ch_mock;
    bool mockSerialized;
    bool mockAtom;

    void setMessageBuilder(shared_ptr<MessageBuilder> mock) {
        m_messageBuilder = mock;
    }

    string sendFuseReceiveSerializedMessage(string messageType, string answerType, string messageInput) {
        if(mockSerialized)
            return mockSerializedFun(messageType, answerType, messageInput);
        else 
            return FslogicProxy::sendFuseReceiveSerializedMessage(messageType, answerType, messageInput);
    }

    string sendFuseReceiveAtomMessage(string messageType, string messageInput) {
        if(mockAtom)
            return mockAtomFun(messageType, messageInput);
        else 
            return FslogicProxy::sendFuseReceiveAtomMessage(messageType, messageInput);
    }

    MOCK_METHOD2(mockAtomFun, string(string, string));
    MOCK_METHOD3(mockSerializedFun, string(string, string, string));
};

#endif // FSLOGIC_PROXY_PROXY_H