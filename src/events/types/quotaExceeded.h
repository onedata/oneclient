/**
 * @file quotaExceeded.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_QUOTA_EXCEEDED_H
#define ONECLIENT_EVENTS_TYPES_QUOTA_EXCEEDED_H

#include "singleEvent.h"

#include <vector>

namespace one {
namespace clproto {
class QuotaExceededEvent;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c QuotaExceeded class represents an exhaustion of available space in the
 * system.
 */
class QuotaExceeded : public SingleEvent {
    using ProtocolMessage = clproto::QuotaExceededEvent;

public:
    /**
     * Constructor.
     * @param msg A Protocol Buffers message representing @c QuotaExceeded
     * counterpart.
     */
    QuotaExceeded(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    /**
     * @return List of spaces where quota have been exceeded.
     */
    const std::vector<std::string> &spaces() const;

    std::string toString() const override;

private:
    std::vector<std::string> m_spaces;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_QUOTA_EXCEEDED_H
