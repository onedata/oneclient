/**
 * @file metadataEventHandler.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "events/eventStream.h"

namespace one {
namespace client {

namespace events {
class EventManager;
} // namespace events

namespace cache {
class LRUMetadataCache;
} // namespace cache

/**
 * @c MetadataEventHandler is responsible for routing events to metadata cache.
 */
class MetadataEventHandler {
public:
    /**
     * Constructor.
     * Sets handlers on @c EventManager instance to handle metadata-related
     * events.
     * @param eventManager The @c EventManager instance.
     * @param metadataCache Cache that will be modified when handling events.
     * @param runInFiber A function that runs callback inside a main fiber.
     */
    MetadataEventHandler(events::EventManager &eventManager,
        cache::LRUMetadataCache &metadataCache,
        std::function<void(folly::Function<void()>)> runInFiber);

    /**
     * Destructor.
     * Unsets all callbacks set by the constructor.
     */
    ~MetadataEventHandler();

private:
    void handleFileAttrs(std::vector<events::FileAttrEventStream::EventPtr>);
    void handleFileLocations(
        std::vector<events::FileLocationEventStream::EventPtr>);
    void handleFileRemovals(
        std::vector<events::FileRemovalEventStream::EventPtr>);
    void handleFileRenames(
        std::vector<events::FileRenamedEventStream::EventPtr>);

    events::EventManager &m_eventManager;
    cache::LRUMetadataCache &m_metadataCache;
};

} // namespace client
} // namespace one
