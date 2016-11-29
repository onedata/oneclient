/**
 * @file keyAggregator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_KEY_AGGREGATOR_H
#define ONECLIENT_EVENTS_KEY_AGGREGATOR_H

#include "aggregator.h"

#include <unordered_map>

namespace one {
namespace client {
namespace events {

class KeyAggregator : public Aggregator {
public:
    void process(ConstEventPtr event) override;

    std::vector<EventPtr> flush() override;

private:
    std::unordered_map<std::string, EventPtr> m_events;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_KEY_AGGREGATOR_H
