/**
 * @file keyAggregator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_AGGREGATORS_KEY_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_KEY_AGGREGATOR_H

#include "aggregator.h"
#include "events/types/event.h"
#include "logging.h"

#include <unordered_map>

namespace one {
namespace client {
namespace events {

template <class T> class KeyAggregator : public Aggregator<T> {
public:
    void process(EventPtr<T> event) override;

    Events<T> flush() override;

private:
    std::unordered_map<std::string, EventPtr<T>> m_events;
};

template <class T> void KeyAggregator<T>::process(EventPtr<T> event)
{
    auto it = m_events.find(event->aggregationKey());
    if (it != m_events.end()) {
        it->second->aggregate(std::move(event));
    }
    else {
        auto key = event->aggregationKey();
        m_events.emplace(std::move(key), std::move(event));
    }
}

template <class T> Events<T> KeyAggregator<T>::flush()
{
    Events<T> events;
    for (auto it = m_events.begin(); it != m_events.end(); ++it) {
        DLOG(INFO) << "Emitting event: " << it->second->toString();
        events.emplace_back(std::move(it->second));
    }
    m_events.clear();
    return events;
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_KEY_AGGREGATOR_H
