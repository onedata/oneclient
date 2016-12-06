/**
 * @file event.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_EVENT_H
#define ONECLIENT_EVENTS_TYPES_EVENT_H

#include "events/declarations.h"

#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace one {
namespace client {
namespace events {

class Event {
public:
    virtual StreamKey streamKey() const = 0;

    virtual const std::string &aggregationKey() const = 0;

    virtual std::string toString() const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_EVENT_H
