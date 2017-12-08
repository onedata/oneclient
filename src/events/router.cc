/**
 * @file router.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events.h"
#include "logging.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"
#include "monitoring/monitoring.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

Router::Router(Manager &manager, communication::Communicator &communicator)
    : m_eventManager{manager}
{
    auto predicate = [](const clproto::ServerMessage &msg, const bool) {
        return msg.has_events() || msg.has_subscription() ||
            msg.has_subscription_cancellation();
    };
    auto callback = [this](const clproto::ServerMessage &msg) {
        if (msg.has_events())
            handle(msg.events());
        else if (msg.has_subscription())
            handle(msg.subscription());
        else if (msg.has_subscription_cancellation())
            handle(msg.subscription_cancellation());
    };
    communicator.subscribe(communication::SubscriptionData{
        std::move(predicate), std::move(callback)});
}

void Router::handle(const ProtoEvents &msg)
{
    for (const auto &eventMsg : msg.events()) {
        if (eventMsg.has_file_attr_changed()) {
            m_eventManager.emit(std::make_unique<FileAttrChanged>(
                eventMsg.file_attr_changed()));
            ONE_METRIC_COUNTER_INC(
                "comp.oneclient.mod.events.submod.emitted.file_attr_changed");
        }
        else if (eventMsg.has_file_location_changed()) {
            m_eventManager.emit(std::make_unique<FileLocationChanged>(
                eventMsg.file_location_changed()));
            ONE_METRIC_COUNTER_INC("comp.oneclient.mod.events.submod.emitted."
                                   "file_location_changed");
        }
        else if (eventMsg.has_file_perm_changed()) {
            m_eventManager.emit(std::make_unique<FilePermChanged>(
                eventMsg.file_perm_changed()));
            ONE_METRIC_COUNTER_INC(
                "comp.oneclient.mod.events.submod.emitted.file_perm_changed");
        }
        else if (eventMsg.has_file_removed()) {
            m_eventManager.emit(
                std::make_unique<FileRemoved>(eventMsg.file_removed()));
            ONE_METRIC_COUNTER_INC(
                "comp.oneclient.mod.events.submod.emitted.file_removed");
        }
        else if (eventMsg.has_file_renamed()) {
            m_eventManager.emit(
                std::make_unique<FileRenamed>(eventMsg.file_renamed()));
            ONE_METRIC_COUNTER_INC(
                "comp.oneclient.mod.events.submod.emitted.file_renamed");
        }
        else if (eventMsg.has_quota_exceeded()) {
            m_eventManager.emit(
                std::make_unique<QuotaExceeded>(eventMsg.quota_exceeded()));
            ONE_METRIC_COUNTER_INC(
                "comp.oneclient.mod.events.submod.emitted.quota_exceeded");
        }
        else {
            DLOG(WARNING) << "Received unhandled event '"
                          << eventMsg.DebugString() << "'";
        }
    }
}

void Router::handle(const ProtoSubscription &msg)
{
    if (msg.has_file_read()) {
        m_eventManager.subscribe(
            msg.id(), FileReadSubscription{msg.file_read()});
    }
    else if (msg.has_file_written()) {
        m_eventManager.subscribe(
            msg.id(), FileWrittenSubscription{msg.file_written()});
    }
    else {
        DLOG(WARNING) << "Received unhandled subscription '"
                      << msg.DebugString() << "'";
    }
}

void Router::handle(const ProtoCancellation &msg)
{
    if (!m_eventManager.unsubscribe(msg.id())) {
        DLOG(WARNING) << "Received unhandled subscription cancellation '"
                      << msg.DebugString() << "'";
    }
}

} // namespace events
} // namespace client
} // namespace one
