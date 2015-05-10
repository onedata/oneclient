/**
* @file eventManager.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_MANAGER_H
#define ONECLIENT_EVENTS_EVENT_MANAGER_H

#include "eventStream.h"

#include <sys/types.h>

#include <memory>
#include <cstddef>
#include <functional>
#include <unordered_map>

namespace one {

namespace clproto {
class ServerMessage;
}

namespace client {

class Context;

namespace events {

class Event;
class ReadEvent;
class WriteEvent;

/**
* The EventManager class is responsible for events management. It handles server
* push messages and provides interface for events emission.
*/
class EventManager {
public:
    /**
    * Constructor.
    * @param context A @c Context instance used to instantiate event streams and
    * acquire communicator instance to register for push messages.
    */
    EventManager(std::shared_ptr<Context> context);

    ~EventManager() = default;

    /**
    * Emits a read event.
    * @param fileId ID of file associated with a read operation.
    * @param offset Distance from the beginning of the file to the first byte
    * read.
    * @param size Number of bytes read.
    */
    void emitReadEvent(
        std::string fileId, off_t offset, std::size_t size) const;

    /**
    * Emits a write event.
    * @param fileId ID of file associated with a write operation.
    * @param offset Distance from the beginning of the file to the first byte
    * written.
    * @param size Number of bytes written.
    * @param fileSize Size of file after a write operation.
    */
    void emitWriteEvent(std::string fileId, off_t offset, std::size_t size,
        off_t fileSize) const;

    /**
    * Emits a truncate event.
    * @param fileId ID of file associated with a truncate operation.
    * @param fileSize Size of file after a truncate operation.
    */
    void emitTruncateEvent(std::string fileId, off_t fileSize) const;

private:
    void handleServerMessage(const clproto::ServerMessage &msg);

    std::shared_ptr<Context> m_context;
    std::unique_ptr<EventStream<ReadEvent>> m_readEventStream;
    std::unique_ptr<EventStream<WriteEvent>> m_writeEventStream;
    std::unordered_map<uint64_t, std::function<void()>>
        m_subscriptionsCancellation;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_MANAGER_H
