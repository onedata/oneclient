/**
* @file writeEventSubscription.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_WRITE_EVENT_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_MESSAGES_WRITE_EVENT_SUBSCRIPTION_H

#include "events.pb.h"

#include <chrono>
#include <boost/optional.hpp>

namespace one {
namespace client {
namespace events {

class WriteEventStream;

static const std::string WRITE_EVENT_SUBSCRIPTION_MESSAGE =
    one::clproto::events::WriteEventSubscription::descriptor()->name();

class WriteEventSubscription {
    friend class WriteEventStream;

public:
    WriteEventSubscription(std::string id);

    const std::string &id() const;

    const boost::optional<size_t> &sizeThreshold() const;

    const boost::optional<size_t> &counterThreshold() const;

    const boost::optional<std::chrono::milliseconds> &timeThreshold() const;

    void setSizeThreshold(size_t sizeThreshold);

    void setCounterThreshold(size_t counterThreshold);

    void setTimeThreshold(const std::chrono::milliseconds &timeThreshold);

    void process(std::weak_ptr<WriteEventStream> stream) const;

private:
    std::string m_id;
    boost::optional<size_t> m_sizeThreshold;
    boost::optional<size_t> m_counterThreshold;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
};

class WriteEventSubscriptionSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    std::unique_ptr<WriteEventSubscription>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif