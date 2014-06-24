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

#include <gmock/gmock.h>

class ProxyFslogicProxy: public veil::client::FslogicProxy
{
public:
    ProxyFslogicProxy(std::shared_ptr<veil::client::Context> context)
        : FslogicProxy{std::move(context)} {}

    bool useMockConnectionSelector;
    boost::shared_ptr<MockCommunicationHandler> ch_mock;
    bool mockAnswer;
    bool mockAtom;
    
    void setMessageBuilder(boost::shared_ptr<veil::client::MessageBuilder> mock)
    {
        m_messageBuilder = mock;
    }

    bool sendFuseReceiveAnswer(const google::protobuf::Message& fMsg, google::protobuf::Message& response) override
    {
        if(mockAnswer)
            return mockAnswerFun(fMsg, response);

        return FslogicProxy::sendFuseReceiveAnswer(fMsg, response);
    }

    std::string sendFuseReceiveAtom(const google::protobuf::Message& fMsg) override
    {
        if(mockAtom)
            return mockAtomFun(fMsg);

        return FslogicProxy::sendFuseReceiveAtom(fMsg);
    }

    MOCK_METHOD1(mockAtomFun, std::string(const google::protobuf::Message&));
    MOCK_METHOD2(mockAnswerFun, bool(const google::protobuf::Message&, google::protobuf::Message&));
};

#endif // FSLOGIC_PROXY_PROXY_H
