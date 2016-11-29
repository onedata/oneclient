/**
 * @file aggregator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_AGGREGATORS_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_AGGREGATOR_H

#include "events/declarations.h"

namespace one {
namespace client {
namespace events {

class Aggregator {
public:
    virtual ~Aggregator() = default;

    virtual void process(ConstEventPtr event) = 0;

    virtual std::vector<EventPtr> flush() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_AGGREGATOR_H
