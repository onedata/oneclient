/**
 * @file pushListener.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "pushListener.h"

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
            onAttr({std::make_unique<clproto::ServerMessage>(msg)});
        else
            onLocation({std::make_unique<clproto::ServerMessage>(msg)});
    };

    m_unsubscribe = communicator.subscribe(
        communication::SubscriptionData(predicate, callback));
}

PushListener::~PushListener() { m_unsubscribe(); }

void PushListener::onAttr(const messages::fuse::FileAttr &msg)
{
    MetadataCache::MetaAccessor acc;
    m_metaCache.getAttr(acc, msg.uuid());

    auto &attr = acc->second.attr.get();
    attr.atime(std::max(attr.atime(), msg.atime()));
    attr.ctime(std::max(attr.ctime(), msg.ctime()));
    attr.mtime(std::max(attr.mtime(), msg.mtime()));
    attr.gid(msg.gid());
    attr.mode(msg.mode());

    if (msg.size() < attr.size()) {
        m_metaCache.getLocation(acc, msg.uuid());
        auto &location = acc->second.location.get();
        location.blocks() &=
            boost::icl::discrete_interval<off_t>::right_open(0, msg.size());
    }

    attr.size(msg.size());
    attr.uid(msg.uid());
}

void PushListener::onLocation(const messages::fuse::FileLocation &msg)
{
    MetadataCache::MetaAccessor acc;
    m_metaCache.getLocation(acc, msg.uuid());
    auto &location = acc->second.location.get();

    location.storageId(msg.storageId());
    location.fileId(msg.fileId());

    // `msg.blocks()` is on LHS of the expression, because FileBlock keeps first
    // value of {storageId, fileId} and ignores any new values
    location.blocks() = msg.blocks() | location.blocks();
}

} // namespace client
} // namespace one
