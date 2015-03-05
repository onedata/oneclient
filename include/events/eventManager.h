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

#include <memory>
#include <sys/types.h>

namespace one {
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
    * Creates a read event.
    * @param fileId ID of file associated with a read operation.
    * @param offset Distance from the beginning of the file to the first byte
    * read.
    * @param size Number of bytes read.
    * @return Unique pointer to read event.
    */
    std::unique_ptr<Event> createReadEvent(const std::string &fileId,
                                           off_t offset, size_t size) const;

    /**
    * Creates a write event.
    * @param fileId ID of file associated with a write operation.
    * @param offset Distance from the beginning of the file to the first byte
    * written.
    * @param size Number of bytes written.
    * @param fileSize Size of file after a write operation.
    * @return Unique pointer to write event.
    */
    std::unique_ptr<Event> createWriteEvent(const std::string &fileId,
                                            off_t offset, size_t size,
                                            off_t fileSize) const;

    /**
    * Creates a truncate event.
    * @param fileId ID of file associated with a truncate operation.
    * @param fileSize Size of file after a truncate operation.
    * @return Unique pointer to truncate event.
    */
    std::unique_ptr<Event> createTruncateEvent(const std::string &fileId,
                                               off_t fileSize) const;

private:
    std::shared_ptr<EventStream<ReadEvent>> m_readEventStream;
    std::shared_ptr<EventStream<WriteEvent>> m_writeEventStream;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_MANAGER_H