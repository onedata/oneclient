/**
 * @file eventManager_proxy.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENT_MANAGER_PROXY_H
#define ONECLIENT_TEST_UNIT_EVENT_MANAGER_PROXY_H

#include "events/eventManager.h"

#include <memory>

class ProxyEventManager : public one::client::events::EventManager {
public:
    ProxyEventManager(std::shared_ptr<one::client::Context> ctx)
        : EventManager{std::move(ctx)}
    {
    }

    void setSubscriptionRegistry(
        std::unique_ptr<one::client::events::SubscriptionRegistry> subReg)
    {
        m_subReg = std::move(subReg);
    }

    void setReadEventStream(std::unique_ptr<one::client::events::IOEventStream<
            one::client::events::ReadEvent>> readEvtStm)
    {
        m_readEvtStm = std::move(readEvtStm);
    }

    void setWriteEventStream(std::unique_ptr<one::client::events::IOEventStream<
            one::client::events::WriteEvent>> writeEvtStm)
    {
        m_writeEvtStm = std::move(writeEvtStm);
    }
};

#endif // ONECLIENT_TEST_UNIT_EVENT_MANAGER_PROXY_H
