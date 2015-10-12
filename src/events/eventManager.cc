/**
 * @file eventManager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/eventManager.h"

#include "context.h"
#include "communication/subscriptionData.h"
#include "events/subscriptions/ioEventSubscription.h"
#include "events/subscriptions/eventSubscriptionCancellation.h"

#include "messages.pb.h"

#include <asio/io_service_strand.hpp>

namespace one {
namespace client {
namespace events {

EventManager::EventManager(std::shared_ptr<Context> ctx)
    : m_ctx{std::move(ctx)}
    , m_evtComm{new EventCommunicator{m_ctx}}
    , m_subReg{new SubscriptionRegistry{m_ctx}}
    , m_readEvtStm{new IOEventStream<ReadEvent>{m_ctx, *m_evtComm}}
    , m_writeEvtStm{new IOEventStream<WriteEvent>{m_ctx, *m_evtComm}}
{
    auto predicate = [](const clproto::ServerMessage &msg, const bool) {
        return msg.has_event_subscription();
    };
    auto callback = [this](
        const clproto::ServerMessage &msg) { handleServerMessage(msg); };
    m_ctx->communicator()->subscribe(communication::SubscriptionData{
        std::move(predicate), std::move(callback)});
}

void EventManager::emitReadEvent(
    off_t offset, size_t size, std::string fileUuid) const
{
    m_ctx->scheduler()->post(
        m_readEvtStm->strand(), [ =, fileUuid = std::move(fileUuid) ] {
            ReadEvent evt{offset, size, std::move(fileUuid)};
            m_readEvtStm->push(std::move(evt));
        });
}

void EventManager::emitWriteEvent(off_t offset, std::size_t size,
    std::string fileUuid, std::string storageId, std::string fileId) const
{
    m_ctx->scheduler()->post(m_writeEvtStm->strand(),
        [
          =,
          fileUuid = std::move(fileUuid),
          storageId = std::move(storageId),
          fileId = std::move(fileId)
        ] {
            WriteEvent evt{offset, size, std::move(fileUuid),
                std::move(storageId), std::move(fileId)};
            m_writeEvtStm->push(std::move(evt));
        });
}

void EventManager::emitTruncateEvent(off_t fileSize, std::string fileUuid) const
{
    m_ctx->scheduler()->post(
        m_writeEvtStm->strand(), [ =, fileUuid = std::move(fileUuid) ] {
            TruncateEvent evt{fileSize, std::move(fileUuid)};
            m_writeEvtStm->push(std::move(evt));
        });
}

void EventManager::handleServerMessage(const clproto::ServerMessage &msg)
{
    const auto &subMsg = msg.event_subscription();
    if (subMsg.has_read_event_subscription()) {
        IOEventSubscription<ReadEvent> sub{subMsg.read_event_subscription()};
        m_ctx->scheduler()->post(
            m_readEvtStm->strand(), [ this, sub = std::move(sub) ] {
                auto record = m_readEvtStm->subscribe(std::move(sub));
                m_subReg->add(std::move(record));
            });
    }
    else if (subMsg.has_write_event_subscription()) {
        IOEventSubscription<WriteEvent> sub{subMsg.write_event_subscription()};
        m_ctx->scheduler()->post(
            m_writeEvtStm->strand(), [ this, sub = std::move(sub) ] {
                auto record = m_writeEvtStm->subscribe(std::move(sub));
                m_subReg->add(std::move(record));
            });
    }
    else if (subMsg.has_event_subscription_cancellation()) {
        EventSubscriptionCancellation subCan{
            subMsg.event_subscription_cancellation()};
        m_subReg->remove(std::move(subCan));
    }
}

} // namespace events
} // namespace client
} // namespace one
