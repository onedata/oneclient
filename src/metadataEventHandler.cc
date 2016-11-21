#include "metadataEventHandler.h"

#include "cache/lruMetadataCache.h"
#include "events/eventManager.h"

namespace one {
namespace client {

MetadataEventHandler::MetadataEventHandler(events::EventManager &eventManager,
    cache::LRUMetadataCache &metadataCache,
    std::function<void(folly::Function<void()>)> runInFiber)
    : m_eventManager{eventManager}
    , m_metadataCache{metadataCache}
{
    m_eventManager.setFileAttrHandler([=](auto events) {
        runInFiber([ this, events = std::move(events) ]() mutable {
            this->handleFileAttrs(std::move(events));
        });
    });

    m_eventManager.setFileLocationHandler([=](auto events) {
        runInFiber([ this, events = std::move(events) ]() mutable {
            this->handleFileLocations(std::move(events));
        });
    });

    m_eventManager.setFileRemovedHandler([=](auto events) {
        runInFiber([ this, events = std::move(events) ]() mutable {
            this->handleFileRemoveds(std::move(events));
        });
    });

    m_eventManager.setFileRenamedHandler([=](auto events) {
        runInFiber([ this, events = std::move(events) ]() mutable {
            this->handleFileRenames(std::move(events));
        });
    });
}

MetadataEventHandler::~MetadataEventHandler()
{
    m_eventManager.setFileAttrHandler([](auto) {});
    m_eventManager.setFileLocationHandler([](auto) {});
    m_eventManager.setFileRemovedHandler([](auto) {});
    m_eventManager.setFileRenamedHandler([](auto) {});
}

void MetadataEventHandler::handleFileAttrs(
    std::vector<events::FileAttrEventStream::EventPtr> events)
{
    for (auto &event : events) {
        auto &attr = event->wrapped();
        if (m_metadataCache.updateFileAttr(attr))
            LOG(INFO) << "Updated attributes for uuid: '" << attr.uuid()
                      << "', size: " << (attr.size() ? *attr.size() : -1);
        else
            LOG(INFO) << "No attributes to update for uuid: '" << attr.uuid()
                      << "'";
    }
}

void MetadataEventHandler::handleFileLocations(
    std::vector<events::FileLocationEventStream::EventPtr> events)
{
    for (auto &event : events) {
        auto &loc = event->wrapped();
        if (m_metadataCache.updateFileLocation(loc))
            LOG(INFO) << "Updated locations for uuid: '" << loc.uuid() << "'";
        else
            LOG(INFO) << "No location to update for uuid: '" << loc.uuid()
                      << "'";
    }
}

void MetadataEventHandler::handleFileRemoveds(
    std::vector<events::FileRemovedEventStream::EventPtr> events)
{
    for (auto &event : events) {
        auto &uuid = event->fileUuid();
        if (m_metadataCache.markDeleted(uuid))
            LOG(INFO) << "File remove event received: " << uuid;
        else
            LOG(INFO) << "Received a file remove event for '" << uuid
                      << "', but the file metadata is no longer cached.";
    }
}

void MetadataEventHandler::handleFileRenames(
    std::vector<events::FileRenamedEventStream::EventPtr> events)
{
    for (auto &event : events) {
        auto &entry = event->topEntry();
        if (m_metadataCache.rename(entry.oldUuid(), entry.newParentUuid(),
                entry.newName(), entry.newUuid()))
            LOG(INFO) << "File renamed event handled: '" << entry.oldUuid()
                      << "' -> '" << entry.newUuid() << "'";
        else
            LOG(INFO) << "Received a file renamed event for '"
                      << entry.oldUuid()
                      << "', but the file metadata is no longer cached.";
    }
}

} // namespace client
} // namespace one
