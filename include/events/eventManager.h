/**
 * @file eventManager.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EVENT_MANAGER_H
#define ONECLIENT_EVENTS_EVENT_MANAGER_H

#include "events/eventCommunicator.h"
#include "events/buffers/ioEventBuffer.h"
#include "events/buffers/voidEventBuffer.h"
#include "events/streams/ioEventStream.h"
#include "events/subscriptionRegistry.h"

#include <sys/types.h>

#include <memory>
#include <cstddef>

namespace one {

namespace clproto {
class ServerMessage;
}

namespace client {

class Context;

namespace events {

/**
 * The EventManager class is responsible for events management. It handles
 * server push messages and provides interface for events emission.
 */
class EventManager {
public:
    /**
     * Constructor.
     * @param context A @c Context instance used to instantiate event streams
     * and
     * acquire communicator instance to register for push messages.
     */
    EventManager(std::shared_ptr<Context> ctx);

    /**
     * Emits a read event.
     * @param fileId ID of file associated with a read operation.
     * @param offset Distance from the beginning of the file to the first byte
     * read.
     * @param size Number of bytes read.
     */
    void emitReadEvent(std::string fileId, off_t offset, std::size_t size);

    /**
     * Emits a write event.
     * @param fileId ID of file associated with a write operation.
     * @param offset Distance from the beginning of the file to the first byte
     * written.
     * @param size Number of bytes written.
     * @param fileSize Size of file after a write operation.
     */
    void emitWriteEvent(
        std::string fileId, off_t offset, std::size_t size, off_t fileSize);

    /**
     * Emits a truncate event.
     * @param fileId ID of file associated with a truncate operation.
     * @param fileSize Size of file after a truncate operation.
     */
    void emitTruncateEvent(std::string fileId, off_t fileSize);

    /**
     * Handles server message.
     * @param msg The server message.
     */
    void handleServerMessage(const clproto::ServerMessage &msg);

protected:
    std::shared_ptr<Context> m_ctx;
    std::unique_ptr<EventCommunicator> m_evtComm;
    std::unique_ptr<SubscriptionRegistry> m_subReg;
    std::unique_ptr<IOEventStream<ReadEvent>> m_readEvtStm;
    std::unique_ptr<IOEventStream<WriteEvent>> m_writeEvtStm;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_MANAGER_H
