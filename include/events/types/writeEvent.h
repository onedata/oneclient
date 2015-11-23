/**
 * @file writeEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H

#include "event.h"

#include "messages/fuse/fileBlock.h"

#include <boost/icl/interval_map.hpp>
#include <boost/optional.hpp>

#include <sys/types.h>

#include <string>
#include <memory>

namespace one {
namespace client {
namespace events {

class WriteEventSubscription;

/**
 * The WriteEvent class represents a write operation in the file system.
 */
class WriteEvent : public Event {
public:
    typedef typename one::client::events::WriteEventSubscription Subscription;
    using FileBlock = messages::fuse::FileBlock;

    /**
     * Default constructor.
     * Creates identity element for write events aggregation operation.
     */
    WriteEvent();

    /**
     * Constructor.
     * @param eventStream Weak pointer to @c WriteEventStream to which this
     * event
     * will be pushed when emitted.
     * @param fileUuid UUID of file associated with a write operation.
     * @param storageId ID of a storage the write has been written to,
     * @param fileId ID of a file on the storage.
     * @param offset Distance from the beginning of the file to the first
     * byte written.
     * @param size Number of bytes written.
     * @param counter Number of write events aggregated in @c this event.
     */
    WriteEvent(off_t offset, std::size_t size, std::string fileUuid,
        std::size_t counter = 1, std::string storageId = {},
        std::string fileId = {});

    virtual ~WriteEvent() = default;

    /**
     * @return UUID of file associated with the write event.
     */
    const std::string &fileUuid() const;

    /**
     * @return Number of bytes written.
     */
    std::size_t size() const;

    /**
     * @return Size of file associated with the write event.
     */
    boost::optional<off_t> fileSize() const;

    /**
     * @return Set of bytes blocks written.
     */
    const auto &blocks() const { return m_blocks; }

    /**
     * Aggregates this write event with an other write event.
     * Aggregates @c this event with an other write event.
     * Aggregation is done by:
     * - addition of events' counters
     * - addition of events' sizes
     * - union of sets of write segments
     * - substitution of file size with file size associated with other event
     * - intersection of write segments and segment with length of file size
     * associated with the truncate event
     * @param event Write event to be aggregated.
     * @return @c *this
     */
    WriteEvent &operator+=(const WriteEvent &event);

    virtual std::string toString() const override;

protected:
    /**
     * @copydoc WriteEvent(off_t, std::size_t, std::string, std::string,
     * std::string, std::size_t)
     * @param fileSize Size of file after a write operation.
     */
    WriteEvent(off_t offset, std::size_t size, off_t fileSize,
        std::string fileUuid, std::size_t counter = 1,
        std::string storageId = {}, std::string fileId = {});

    std::string m_fileUuid;
    std::size_t m_size = 0;
    boost::optional<off_t> m_fileSize;
    boost::icl::interval_map<off_t, FileBlock, boost::icl::partial_enricher>
        m_blocks;

private:
    std::unique_ptr<one::messages::ProtocolClientMessage>
    serializeAndDestroy() override;
};

/**
 * Compares two write events.
 * Write events are equal if corresponding event's fields are equal.
 * @param lhs Write event to be compared.
 * @param rhs Write event to be compared.
 * @return true if write events are equal and false otherwise.
 */
bool operator==(const WriteEvent &lhs, const WriteEvent &rhs);

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H
