/**
 * @file pushListener.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "pushListener.h"

#include "logging.h"
#include "cache/metadataCache.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"

namespace one {
namespace client {

PushListener::PushListener(
    communication::Communicator &communicator, MetadataCache &metaCache)
    : m_metaCache{metaCache}
{
    auto predicate = [](const clproto::ServerMessage &msg, const bool handled) {
        if (handled || msg.has_message_id() || !msg.has_fuse_response())
            return false;

        return msg.fuse_response().has_file_attr() ||
            msg.fuse_response().has_file_location();
    };

    auto callback = [this](const clproto::ServerMessage &msg) {
        if (msg.fuse_response().has_file_attr())
            onPushAttr({std::make_unique<clproto::ServerMessage>(msg)});
        else
            onPushLocation({std::make_unique<clproto::ServerMessage>(msg)});
    };

    m_unsubscribe = communicator.subscribe(
        communication::SubscriptionData(predicate, callback));
}

PushListener::~PushListener() { m_unsubscribe(); }

void PushListener::onPushAttr(const messages::fuse::FileAttr &msg)
{
    MetadataCache::MetaAccessor acc;
    if (!m_metaCache.get(acc, msg.uuid()) || !acc->second.attr) {
        LOG(INFO) << "No attributes to update for uuid: '" << msg.uuid() << "'";
        return;
    }

    LOG(INFO) << "Updating attributes for uuid: '" << msg.uuid() << "'";
    auto &attr = acc->second.attr.get();

    if (msg.size() < attr.size() && acc->second.location) {
        LOG(INFO) << "Truncating blocks attributes for uuid: '" << msg.uuid()
                  << "'";

        acc->second.location.get().blocks() &=
            boost::icl::discrete_interval<off_t>::right_open(0, msg.size());
    }

    attr.atime(std::max(attr.atime(), msg.atime()));
    attr.ctime(std::max(attr.ctime(), msg.ctime()));
    attr.mtime(std::max(attr.mtime(), msg.mtime()));
    attr.gid(msg.gid());
    attr.mode(msg.mode());
    attr.size(msg.size());
    attr.uid(msg.uid());
}

void PushListener::onPushLocation(const messages::fuse::FileLocation &msg)
{
    MetadataCache::MetaAccessor acc;
    if (!m_metaCache.get(acc, msg.uuid()) || !acc->second.attr) {
        LOG(INFO) << "No location to update for uuid: '" << msg.uuid() << "'";
        return;
    }

    LOG(INFO) << "Updating location for uuid: '" << msg.uuid() << "'";
    auto &location = acc->second.location.get();

    location.storageId(msg.storageId());
    location.fileId(msg.fileId());

    // `msg.blocks()` is on LHS of the expression, because FileBlock keeps first
    // value of {storageId, fileId} and ignores any new values
    location.blocks() = msg.blocks() | location.blocks();
}

} // namespace client
} // namespace one
