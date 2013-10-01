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
    bool mockAnswer;
    bool mockAtom;
    
    void setMessageBuilder(shared_ptr<MessageBuilder> mock) {
        m_messageBuilder = mock;
    }

    bool sendFuseReceiveAnswer(const google::protobuf::Message& fMsg, google::protobuf::Message& response) {
        if(mockAnswer)
            return mockAnswerFun(fMsg, response);
        else 
            return FslogicProxy::sendFuseReceiveAnswer(fMsg, response);
    }

    string sendFuseReceiveAtom(const google::protobuf::Message& fMsg) {
        if(mockAtom)
            return mockAtomFun(fMsg);
        else 
            return FslogicProxy::sendFuseReceiveAtom(fMsg);
    }

    MOCK_METHOD1(mockAtomFun, string(const google::protobuf::Message&));
    MOCK_METHOD2(mockAnswerFun, bool(const google::protobuf::Message&, google::protobuf::Message&));
};

#endif // FSLOGIC_PROXY_PROXY_H