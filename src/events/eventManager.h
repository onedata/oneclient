/**
 * @file eventManager.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EVENT_MANAGER_H
#define ONECLIENT_EVENTS_EVENT_MANAGER_H

#include "events/eventStream.h"
#include "subscriptionContainer.h"

#include <sys/types.h>

#include <cstddef>
#include <memory>
#include <string>

namespace one {
namespace clproto {
class Events;
class Subscription;
class SubscriptionCancellation;
} // namespace clproto
namespace client {
class Context;
namespace events {

class SubscriptionRegistry;

/**
 * @c EventManager class is responsible for events management. It handles
 * server push messages and provides interface for events emission and
 * subscription.
 */
class EventManager {
public:
    /**
     * Constructor.
     * @param context @c Context instance used to instantiate event streams
     * and acquire communicator instance to register for push messages.
     */
    EventManager(std::shared_ptr<Context> context);

    virtual ~EventManager() = default;

    /**
     * Emits a read event.
     * @param offset Distance from the beginning of the file to the first byte
     * read.
     * @param size Number of bytes read.
     * @param fileUuid UUID of file associated with a read operation.
     */
    void emitReadEvent(
        off_t offset, std::size_t size, std::string fileUuid) const;

    /**
     * Emits a write event.
     * @param offset Distance from the beginning of the file to the first byte
     * written.
     * @param size Number of bytes read.
     * @param fileUuid UUID of a file associated with a write operation.
     * @param storageId ID of a storage where a write operation occurred.
     * @param fileId ID of a file on the storage where a write operation
     * occurred.
     */
    void emitWriteEvent(off_t offset, std::size_t size, std::string fileUuid,
        std::string storageId, std::string fileId) const;

    /**
     * Emits a truncate event.
     * @param fileSize Size of file after a truncate operation.
     * @param fileUuid UUID of file associated with a truncate operation.
     */
    void emitTruncateEvent(off_t fileSize, std::string fileUuid) const;

    /**
     * Sets handler for file atr update events.
     * @param handler Handler to be set.
     */
    void setFileAttrHandler(typename FileAttrEventStream::Handler handler);

    /**
     * Adds subscription for file attr changes.
     * @param clientSubscription Client side subscription parameters.
     * @param serverSubscription Server side subscription parameters.
     * @return Subscription ID.
     */
    std::int64_t subscribe(FileAttrSubscription clientSubscription,
        FileAttrSubscription serverSubscription);

    /**
     * Sets handler for file location update events.
     * @param handler Handler to be set.
     */
    void setFileLocationHandler(
        typename FileLocationEventStream::Handler handler);

    /**
     * Sets handler for permission changed events.
     * @param handler Handler to be set.
     */
    void setPermissionChangedHandler(
        PermissionChangedEventStream::Handler handler);

    /**
     * Sets handler for removing file events.
     * @param handler Handler to be set.
     */
    void setFileRemovalHandler(FileRemovalEventStream::Handler handler);

    /**
     * Emits a remove file event.
     * @param fileUuid UUID of removed file.
     */
    void emitFileRemovalEvent(std::string fileUuid) const;

    /**
     * Adds subscription for removing file events.
     * @param clientSubscription Client side subscription parameters.
     * @param serverSubscription Server side subscription parameters.
     * @return Subscription ID.
     */
    std::int64_t subscribe(FileRemovalSubscription clientSubscription,
        FileRemovalSubscription serverSubscription);

    /**
     * Adds subscription for permission changeg events.
     * @param clientSubscription Client side subscription parameters.
     * @param serverSubscription Server side subscription parameters.
     * @return Subscription ID.
     */
    std::int64_t subscribe(FileLocationSubscription clientSubscription,
        FileLocationSubscription serverSubscription);
    /**
     * Adds subscription for perssion changes.
     * @param clientSubscription Client side subscription parameters.
     * @param serverSubscription Server side subscription parameters.
     * @return Subscription ID.
     */
    std::int64_t subscribe(PermissionChangedSubscription clientSubscription,
        PermissionChangedSubscription serverSubscription);

    /**
     * Adds server subscriptions.
     * @param container Container with server subscriptions.
     */
    void subscribe(SubscriptionContainer container);

    /**
     * Removes subscription.
     * @param id ID of subscription to be removed.
     * @return 'true' if subscription was successfully removed, otherwise
     * 'false'.
     */
    bool unsubscribe(std::int64_t id);

    /**
     * Handles events message.
     * @param message @c one::clproto::Events instance.
     */
    void handle(const clproto::Events &message);

    /**
     * Handles event subscription message.
     * @param message @c one::clproto::Subscription instance.
     */
    void handle(const clproto::Subscription &message);

    /**
     * Handles event subscription cancellation message.
     * @param message T@c one::clproto::SubscriptionCancellation instance.
     */
    void handle(const clproto::SubscriptionCancellation &message);

    /**
     * @return @c SubscriptionRegistry instance
     */
    std::shared_ptr<SubscriptionRegistry> subscriptionRegistry() const;

protected:
    communication::StreamManager m_streamManager;
    std::shared_ptr<SubscriptionRegistry> m_registry;
    std::unique_ptr<ReadEventStream> m_readEventStream;
    std::unique_ptr<WriteEventStream> m_writeEventStream;
    std::unique_ptr<FileAttrEventStream> m_fileAttrEventStream;
    std::unique_ptr<FileLocationEventStream> m_fileLocationEventStream;
    std::unique_ptr<PermissionChangedEventStream>
        m_permissionChangedEventStream;
    std::unique_ptr<FileRemovalEventStream> m_fileRemovalEventStream;

private:
    void initializeStreams(std::shared_ptr<Context> context);
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_MANAGER_H
