/**
 * @file falseEmitter.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EMITTERS_FALSE_EMITTER_H
#define ONECLIENT_EVENTS_EMITTERS_FALSE_EMITTER_H

#include "emitter.h"

namespace one {
namespace client {
namespace events {

class FalseEmitter : public Emitter {
    void process(ConstEventPtr event) override;

    bool ready() override;

    void reset() override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_FALSE_EMITTER_H
