/**
* @file eventFactory.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_FACTORY_H
#define ONECLIENT_EVENTS_EVENT_FACTORY_H

#include <memory>

namespace one {
namespace client {
namespace events {

class ReadEvent;
class WriteEvent;
class ReadEventStream;
class WriteEventStream;
class SubscriptionEvent;
class SubscriptionCancellationEvent;

/**
* The EventFactory class is responsible for creating event instances.
*/
class EventFactory {
public:
    /**
    * Creates a read event.
    * @param fileId ID of file associated with a read operation.
    * @param offset Distance from the beginning of the file to the first byte
    * read.
    * @param size Amount of bytes read.
    * @param stream A @c ReadEventStream instance.
    * @return Returns a @c ReadEvent instance.
    */
    std::unique_ptr<ReadEvent>
    createReadEvent(const std::string &fileId, off_t offset, size_t size,
                    std::weak_ptr<ReadEventStream> stream) const;

    /**
    * Creates a write event.
    * @param fileId ID of file associated with a write operation.
    * @param offset Distance from the beginning of the file to the first byte
    * write.
    * @param size Amount of bytes write.
    * @param fileSize Size of file associated with a write operation.
    * @param stream A @c WriteEventStream instance.
    * @return Returns a @c WriteEvent instance.
    */
    std::unique_ptr<WriteEvent>
    createWriteEvent(const std::string &fileId, off_t offset, size_t size,
                     off_t fileSize,
                     std::weak_ptr<WriteEventStream> stream) const;

    /**
    * Creates a truncate event.
    * @param fileId ID of file associated with a write operation.
    * @param fileSize Size of file associated with a write operation.
    * @param stream A @c WriteEventStream instance.
    * @return Returns a @c WriteEvent instance.
    */
    std::unique_ptr<WriteEvent>
    createTruncateEvent(const std::string &fileId, off_t fileSize,
                        std::weak_ptr<WriteEventStream> stream) const;

    /**
    * Creates a subscription event.
    * @param id ID of added subscription.
    * @return Returns a @c SubscriptionEvent instance.
    */
    std::unique_ptr<SubscriptionEvent>
    createSubscriptionEvent(unsigned long long id) const;

    /**
    * Creates a subscription cancellation event.
    * @param id ID of cancelled subscription.
    * @return Returns a @c SubscriptionCancellationEvent instance.
    */
    std::unique_ptr<SubscriptionCancellationEvent>
    createSubscriptionCancellationEvent(unsigned long long id) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_FACTORY_H